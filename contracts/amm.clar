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
;;

;; private functions
;;helper fnc to ensure that the token0 principal address is less than the token1 principal
(define-private (correct-token-ordering (token-0 principal) (token-1 principal)) 
    
    (let (
        (token-0-buff (unwrap-panic (to-consensus-buff? token-0)))
        (token-1-buff (unwrap-panic (to-consensus-buff? token-1)))
    )
    (asserts! (< token-0-buff token-1-buff) (err u201))
    (ok true)
    )
)
;;

