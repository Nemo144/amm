;; title: amm
;; version:
;; summary:
;; description:

;; traits
;;define the standard sip-010-trait for creating tokens
(use-trait ft-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)
;;

;; token definitions
;;

;; constants

;;minimum liquidity that must exist in a created pool
(define-constant MINIMUM_LIQUIDITY u1000)

;;this contract
(define-constant THIS_CONTRACT (as-contract tx-sender))

;;fees denominator
(define-constant FEES_DENOM u10000)

;;errors
(define-constant ERR_POOL_ALREADY_EXISTS (err u200)) ;; pool already exists
(define-constant ERR_INCORRECT_TOKEN_ORDERING (err u201)) ;; incorrect token ordering (invalid sorting)
(define-constant ERR_INSUFFICIENT_LIQUIDITY_MINTED (err u202)) ;; insufficient liquidity amounts being added
(define-constant ERR_INSUFFICIENT_LIQUIDITY_OWNED (err u203)) ;; not enough liquidity owned to withdraw the requested amount
(define-constant ERR_INSUFFICIENT_LIQUIDITY_BURNED (err u204)) ;; insufficient liquidity amounts being removed
(define-constant ERR_INSUFFICIENT_INPUT_AMOUNT (err u205)) ;; insufficient input token amount for swap
(define-constant ERR_INSUFFICIENT_LIQUIDITY_FOR_SWAP (err u206)) ;; insufficient liquidity in pool for swap
(define-constant ERR_INSUFFICIENT_1_AMOUNT (err u207)) ;; insufficient amount of token 1 for swap
(define-constant ERR_INSUFFICIENT_0_AMOUNT (err u208)) ;; insufficient amount of token 0 for swap
;;

;; data vars
;;

;; data maps
(define-map pools 
    (buff 20) ;; pool ID => (hash of Token0 + Token1 + fee)
    {
        token-0: principal,
        token-1: principal,
        fee: uint,

        liquidity: uint,
        balance-0: uint,
        balance-1: uint,
    }
)

(define-map positions 
    {
        pool-id: (buff 20),
        owner: principal,
    }
    {
        liquidity: uint
    }
)
;;

;; public functions
;;define the create-pool function
(define-public (create-pool (token-0 <ft-trait>) (token-1 <ft-trait>) (fee uint))

    (let (
        ;;create the pool info tuple
        (pool-info {
            token-0: token-0,
            token-1: token-1,
            fee: fee
        })

        ;;define the pool-id
        (pool-id (get-pool-id pool-info))

        ;;confirm that the pool-id does not already exist
        (pool-does-not-exist (is-none (map-get? pools pool-id)))

        ;;convert the ft-trait to principals
        (token-0-principal (contract-of token-0))
        (token-1-principal (contract-of token-1))

        ;;create the pool data
        (pool-data {
            token-0: token-0-principal,
            token-1: token-1-principal,
            fee: (get fee pool-info),
            liquidity: u0,;;initial value
            balance-0: u0,;;initial value
            balance-1: u0,;;initial value
        })

        )
        
        ;;check that the pool does not exist
        (asserts! pool-does-not-exist (err u200))

        ;;check that the token ordering is correct
        (asserts! (is-ok (correct-token-ordering token-0-principal token-1-principal)) (err u201))

        ;;update the pools mapping
        (map-set pools pool-id pool-data)

        ;;print the action and data
        (print {action: "create-pool", data: pool-data})

        ;;return ok
        (ok true)
    )

)
;;

;; read only functions

;;compute the hash of token0 + token1 + fee to use as poolID
(define-read-only (get-pool-id
    (pool-info {token-0: <ft-trait>, token-1: <ft-trait>, fee: uint}))

    (let (
        (buff (unwrap-panic (to-consensus-buff? pool-info)));;convert the tuple => pool-info into a buffer(raw bytes)
        (pool-id (hash160 buff));; take the hash of the raw bytes which becomes the pool-id
        )
        pool-id
    )
)

;;get-position-liquidity function will fetch a user's presupplied liquidity to the pool if any otherwise returns 0
(define-read-only (get-position-liquidity (pool-id (buff 20)) (owner principal))
    (let (
        ;;check the position from the 'positions' map
        (position (map-get? positions {pool-id: pool-id, owner: owner}))

        ;;check existing liquidity of the owner if any otherwise return 0
        (existing-owner-liquidity (if (is-some position) (unwrap-panic position) {liquidity: u0}))
        
        )

        (ok (get liquidity existing-owner-liquidity))
    )
)
;;

;; private functions

;;helper fnc to ensure that the token0 principal address is less than the token1 principal
(define-private (correct-token-ordering (token-0 principal) (token-1 principal)) 
    
    (let (
        (token-0-buff (unwrap-panic (to-consensus-buff? token-0))) ;;converts token-0 from principal to buffer
        (token-1-buff (unwrap-panic (to-consensus-buff? token-1))) ;;converts token-1 from principal to buffer
    )
    (asserts! (< token-0-buff token-1-buff) (err u201))
    (ok true)
    )
)

;;get-amounts function calculates how much of each tokens will be added as liquidity within the specified constraints
(define-private (get-amounts (amount-0-desired uint) (amount-1-desired uint) (amount-0-min uint) (amount-1-min uint) (balance-0 uint) (balance-1 uint)) 
    
    (let (
        ;;calculate the ideal amount of token-1 to be provided that evens out the ratio of the pool with respect to the `amount-0-desired`
        (amount-1-given-0 (/ (* amount-0-desired balance-1) balance-0))

        ;;calculate the ideal amount of token-0 to be provided that evens out the ratio of the pool with respect to the `amount-1-desired`
        (amount-0-given-1 (/ (* amount-1-desired balance-0) balance-1))
        )

        (if
            ;;if ideal amount-1 is less than amount-1 desired
            (<= amount-1-given-0 amount-1-desired)
            (begin 

            ;;make sure the ideal amount-1 is greater or equal to amount-1-minimum otherwise throw an error
            (asserts! (>= amount-1-given-0 amount-1-min) (err u207))

            ;;amount-0-desired and ideal amount-1 can be added to the pool
            (ok {amount-0: amount-0-desired, amount-1: amount-1-given-0})
            )

            ;;else if the ideal amount-1 is greater than the desired amount-1, we can only add up to 'amount-1-desired' to the pool

            (begin

            ;;make sure the ideal amount-0 is less than the desired amount-0 otherwise throw an error
            (asserts! (<= amount-0-given-1 amount-0-desired) (err u208)) 

            ;;make sure the ideal amount-0 >= the amount-0-minimum
            (asserts! (>= amount-0-given-1 amount-0-min) (err u208))

            ;;add the ideal amount-0 and amount-1-desired to the pool
            (ok {amount-0: amount-0-given-1, amount-1: amount-1-desired})
            )
        )
    )
)
;;

