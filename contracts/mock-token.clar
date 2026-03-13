;; title: mock-token
;; version:
;; summary:
;; description:

;; traits
;;implement the sip-010-trait for creating fungible tokens
(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)
;;

;; token definitions
;;

;; constants
;;define the contract-owner
(define-constant contract-owner tx-sender)

;;define the error codes 
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))

;;define the fungible token without maximum supply
(define-fungible-token mock-token )
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;define the transfer function
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        ;;assert that the contract-owner is the sender
        (asserts! (is-eq tx-sender sender) (err u101))

        ;;transfer mock-token from the sender to the principal
        (try! (ft-transfer? mock-token amount sender recipient))

        ;;print the memo
        (match memo to-print (print to-print) 0x)

        ;;return ok
        (ok true)
    )
)

;;define the mint function
(define-public (mint (amount uint) (recipient principal)) 
    (ft-mint? mock-token amount recipient)
)
;;

;; read only functions
;;define the read on;y functions for the fungible-token

(define-read-only (get-name) 
    (ok "Mock Token")
)

(define-read-only (get-symbol)
    (ok "mk")
)

(define-read-only (get-decimals) 
    (ok u6)
)

(define-read-only (get-balance (who principal)) 
    (ok (ft-get-balance mock-token who))
)

(define-read-only (get-total-supply) 
    (ok (ft-get-supply mock-token))
)

(define-read-only (get-token-uri) 
    (ok none)
)
;;

;; private functions
;;

