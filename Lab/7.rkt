;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./hashtable-extras.rkt")

;; hash-update : [Hash-table-of X Y] X (Y -> Y) Y
;; updates entry using function if present, else default

(check-expect (hash-update (make-hash (list)) "foo" add1 0)
              (make-hash (list (list "foo" 0))))
(check-expect (hash-update (make-hash (list (list "foo" 0) (list "bar" 0)))
                           "foo" add1 0)
              (make-hash (list (list "foo" 1) (list "bar" 0))))

(define (hash-update h k upd def)
  (hash-set h k (if (hash-has-key? h k) (upd (hash-ref h k)) def)))

(define-struct transaction [serial sender-sig sender-key receiver-key amount])
;; A Transaction is a (make-transaction Nat Signature PublicKey PublicKey Nat)

(define-struct block [transactions nonce miner-key])
;; A Block is a (make-block [List-of Transaction] Nat PublicKey)

;; A Ledger is a [Hash-Table-of PublicKey Nat]
;; A ledger maps wallet IDs (public keys) to the number of accelcoins they have.

;; reward : PublicKey Ledger -> Ledger
;; Grants the miner the reward for mining a block.
(define (reward PubKey Ledger)
  (hash-update Ledger PubKey (lambda (x) (+ 100 x)) 100)
  )

;; update-ledger/transaction: Transaction Ledger -> [Optional Ledger]
;; Updates the ledger with a single transaction. Produces #false if
;; the sender does not have enough accelcoin to send.
(define (update-ledger/transaction transact Ledger)
  (hash-update Ledger (transaction-sender-key transact) (cond
                                                         [(> (transaction-amount transact)
                                                              (hash-ref Ledger (transaction-sender-key transact)))
                                                           (lambda (x) #false)]
                                                          [else (- (transaction-amount transact)
                                                                   (hash-ref Ledger (transaction-sender-key transact)))]
                                                          )))
   

;; update-ledger/block : Block Ledger -> [Optional Ledger]
;; Updates the ledger with the transactions in a block, and rewards the miner.
(define (update-ledger/block block Ledger)
  (cond
    [(empty? block) null]
    [else
     (update-ledger/block (rest block) (update-ledger/transaction (first block) Ledger))]))

;; update-ledger/blockchain : Blockchain Ledger -> [Optional Ledger]
;; Produces the ledger for a blockchain, or #false if any transaction is invalid
(define (update-ledger/blockchain blockchain Ledger)
  (cond
    [(empty? blockchain) #false]
    [else
     (update-ledger/blockchain (rest blockchain)
                               (lambda (x) 
                               (update-ledger/block (rest x) (update-ledger/transaction (first x) Ledger)))
                               (first blockchain))]))