;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname CS2500_HW10rkt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./hashtable-extras.rkt")
(require "./crypto-extras.rkt")
(require "./http-extras.rkt")

;; A [Hash-Table-of X Y] is a (make-hash (list (list X Y) ...))
;; where X is the key and Y is the value that the key maps to

;; make-hash : [List-of [List-of-Two X Y]] -> [Hash-table-of X Y]
;; Creates a hash table from a list of pairs.

;; hash-has-key? : [Hash-table-of X Y] X -> Boolean
;; Checks if a hash table has a key. Returns #true if it does, #false otherwise.

;; hash-ref : [Hash-table-of X Y] X -> Y
;; Returns the value associated with the key in the hash table.

;; hash-set : [Hash-table-of X Y] X Y -> [Hash-table-of X Y]
;; Returns a new hash table with the key mapped to the value.

;; hash-remove : [Hash-table-of X Y] X -> [Hash-table-of X Y]
;; Returns a new hash table with the key removed

;; hash-keys : [Hash-table-of X Y] -> [List-of X]
;; Returns a list of all the keys in the hash table

;; hash-values : [Hash-table-of X Y] -> [List-of Y]
;; Returns a list of all the values in the hash table

;; =================================

;; A PrivateKey is a String that represents a 512-bit RSA private key.

;; A PublicKey is a String that represents a 512-bit RSA public key.

;; A Signature is a String that represents a 512-bit RSA signature.

;; digest : String -> Nat
;; Produces the SHA256 digest of the given string. SHA256 is a cryptographic
;; hash function that is used in many blockchains, and we will use it too.

;; secret->public : PrivateKey -> PublicKey
;; Generates a public key from a private key.

;; make-signature : String PrivateKey -> Signature
;; Signs a string with a private key.

;; check-signature : PublicKey String Signature -> Boolean
;; Checks if the given string was signed by the given public key.

;; =================================

;; An [Optional X] is one of:
;; - X
;; - #false
;;
;; Interpretation: Either a value of type X or #false

;; =================================

(define-struct transaction [serial unique-string sender-sig sender-key receiver-key amount])
;; A Transaction is a (make-transaction Nat String Signature PublicKey PublicKey Nat)
;;
;; (make-transaction serial unique-string sender-sig sender-key receiver-key amount) represents a
;; single transaction that moves amount accelcoin from sender-key to receiver-key.
;; Moreover:
;;
;; 1. The amount must be positive;
;; 2. The unique-string must be globally unique;
;; 2. The signature signs the string
;;      (string-append unique-string receiver-key ":" (number->string amount))
;;    with the private key corresponding to sender-key.
;; 3. the unique-string is a string that is unique to this transaction.

(define-struct block [transactions nonce miner-key])
;; A Block is a (make-block [List-of Transaction] Nat PublicKey)
;;
;; (make-block transactions nonce miner-key) represents a block of transactions mined by miner-key.
;; The transactions are processed left-to-right. Thus (first transactions) occurs before
;; (second transactions).

;; A Blockchain is a [NE-List-of Block]
;;
;; The first element of a Blockchain is the latest block and the last element is the first
;; block or the *genesis block*. The genesis block has zero transactions and all other blocks have
;; three or more transactions.

(define ALICE-PRIVATE-KEY
  (string-append "MIIBOgIBAAJBAMrPOfefdvowOwAplxY/NLkJFymyedikvwvsyhtQ98CawNXeKydg+WYD9YzQ"
                 "W1tIY5Ta1bqZhk5hpWGM4eusKxkCAwEAAQJAMtQW2hmsLu3xi4vg4uF6bDl8BaZGZWZ8vxdc"
                 "W9ZCEZIEtnYGlkpwoG5YcUp3a39YRP+Nt2fA6bmPbvjmWAspkQIhAPodYjlh0p7P4QodsvQi"
                 "nMRp9Z8knfBmYeQpg/0otBMVAiEAz5Tjyw0Yanh6CCIvDRKQ+SvdTMvrJykIMyzmsWgYSPUC"
                 "IEwGvIG2w3/0rnIVvvzIvKBTmQ7L4ZpedKkXGYDNa5dVAiAfRL5Lh911rFA1iXCs927/Gaxs"
                 "NQtnCrdBfjIB5zxBQQIhAO0ZN+PGdjJfbhivUdgfx+DbrHkClSWT8SidILAbgQkd"))
(define BOB-PRIVATE-KEY
  (string-append "MIIBOwIBAAJBAKy4zO2w1HfXMNHSCYKuheD+5ZkAlHubePYNOVvi3gA/AQ1S0HcRFmTk"
                 "zFz/SCp+0cZ3wErzHhKXmvgIrjLbdYMCAwEAAQJACBwBGyPTRfEnjKJk6erRxFeTZhS"
                 "d5BPPoRXL3KGRNMesv5qct9QNbHA2ghjY4Z1gokwLgCViG88FvG0qMKGNSQIhANduvtUGG"
                 "vqeb+c6khwi60sf/3KMa082IjC3fe4RosJPAiEAzT8eusKDsL3q38i1o6E4pzUuW4oK0ta1BCGEdZn"
                 "2kI0CIDb6bz8ECNyOlHZJL0J48t1ANDuydCxJ313ZZgzceVHnAiEApVA7vg"
                 "1B6K9vaIPO2VbXvMW26wAKq7tH3WXpvJcf41kCIQCTv8zWOp8Dq3NKTdFZD2"
                 "8NCohpiEOAP3yMng9HhXcAqg=="))
(define ALICE-PUBLIC-KEY (secret->public ALICE-PRIVATE-KEY))
(define BOB-PUBLIC-KEY (secret->public BOB-PRIVATE-KEY))

(define PRIVATE-KEY-1
  "MIIBOwIBAAJBAK59eNiP8RRwaa0y34piNlvYOyFDXtRf8NtneIoL8JMMvfVfH6X9a/zzIxHKxIlcY7WZCMVCN7+yU2L0uFE8zT8CAwEAAQJAR3HUk0eMvIOyoVodMXo5K64DuADvySFna06Yil2NKYf2iG6wttj5I2LoPHnK2EfaGGXelt1NBBCTobiJvlGfgQIhAOO22GUjOzNeG2iJO+DsUOijB5poPYIuP3sxBXPJLIpfAiEAxCojCtL0vvExVUylVYC9/f+8d37s0meNXnXIT5cCaSECIQCTJh7kDEY/RXrUrInjKRKHooJRRzcoHparFtx9G+0KZQIgO2+M+I2cjHImAXNcsQB2EIy4/PffJ40qn1FOpRbbKyECIQC9GErNEnR1dWL66hlxqDr67drQghpBgPkcKGYWW4qY7Q==")

(define PUBLIC-KEY-1 (secret->public PRIVATE-KEY-1))

;; build-transaction: Nat PrivateKey PublicKey Nat -> Transaction
;; (build-transaction serial sender-private-key receiver-public-key amount) builds a transaction
;; that sends amount from the sender to the receiver.
(define (build-transaction serial priv pub amt)
  (local ((define str (unique-string)))
    (make-transaction serial
                      str
                      (make-signature (string-append str pub ":" (number->string amt)) priv)
                      (secret->public priv)
                      pub
                      amt)))

;; Sends 100 accelcoins from Alice to Bob
(define EX-TRANSACTION-0 (build-transaction 0 ALICE-PRIVATE-KEY BOB-PUBLIC-KEY 100))

;; transaction->string : Transaction -> String
;; Serializes a transaction into a string with the format
;; "serial:transaction:unique-string:sender-sig:sender-key:receiver-key,amount"
(define (transaction->string t)
  (string-append (number->string (transaction-serial t))
                 ":transaction:"
                 (transaction-unique-string t)
                 ":"
                 (transaction-sender-sig t)
                 ":"
                 (transaction-sender-key t)
                 ":"
                 (transaction-receiver-key t)
                 ","
                 (number->string (transaction-amount t))))

;; transaction2->string : Transaction -> String
;; Serializes a transaction into a string with the format
;; "transaction:unique-string:sender-sig:sender-key:receiver-key,amount"
(define (transaction->string2 t)
  (string-append "transaction:"
                 (transaction-unique-string t)
                 ":"
                 (transaction-sender-sig t)
                 ":"
                 (transaction-sender-key t)
                 ":"
                 (transaction-receiver-key t)
                 ","
                 (number->string (transaction-amount t))))

;; block-digest: Digest Block -> Digest
;; (block-digest prev-digest block) computes the digest of block, given the digest
;; of the previous block.
;;
;; The digest must be the digest of the following strings concatenated in order:
;;
;; 1. prev-digest as a string
;; 2. The transactions as strings (using transaction->string) concatenated in order
;; 3. The nonce as a string
(define (block-digest prev-digest block)
  (digest
   (string-append
    (number->string prev-digest)
    (foldr (lambda (x y) (string-append (transaction->string x) y)) "" (block-transactions block))
    (number->string (block-nonce block)))))

;; The limit digest value
(define DIGEST-LIMIT (expt 2 (* 8 30)))

;; mine-block : Digest PublicKey [List-of Transaction] Nat -> [Optional Block]
;; (mine-block prev-digest miner-public-key transactions trials)
;; tries to mine a block, but gives up after trials attempts.
;;
;; The produced block has a digest that is less than DIGEST-LIMIT.
(define (mine-block prev-digest miner-public-key transactions trials)
  (cond
    [(= trials 0) #false]
    [else
     (local [(define generated-block (make-block transactions (random 4294967087) miner-public-key))]
       (if (< (block-digest prev-digest generated-block) DIGEST-LIMIT)
           generated-block
           (mine-block prev-digest miner-public-key transactions (- trials 1))))]))

;; -------------Blockchain validation------------------------
;; blocks-sized-ok? : Blockchain -> Boolean
;; Determines that every block has at least three transactions, and that
;; the genesis block has zero transactions.
(define (blocks-sized-ok? bc)
  (and (empty? (block-transactions (first (reverse bc))))
       (andmap (lambda (x) (>= (length (block-transactions x)) 3)) (rest (reverse bc)))))

#;(check-expect (blocks-sized-ok? EX-BC-0) #true)
#;(check-expect (blocks-sized-ok? EX-BC-1) #false)

;; map-transactions : (Transaction -> X) Blockchain -> [List-of X]
;; Transaction-level map for blockchains.
(define (map-transactions f bc)
  (foldr (lambda (x y) (append (map f (block-transactions x)) y)) empty bc))

#;(check-expect (map-transactions transaction-serial EX-BC-0) (list 0 1 2 3 4))
#;(check-expect (map-transactions transaction-amount EX-BC-0) (list 10 10 10 10 10))

;; no-duplicate-transactions? : Blockchain -> Boolean
;; Determines if every transaction in the blockchain appears exactly once. Every
;; transaction has a unique serial number that we use to determine if it is unique.
(define (no-duplicate-transactions? bc)
  (local ((define serial-list (sort (map-transactions transaction-serial bc) <)))
    (if (empty? serial-list)
        #t
        (not (ormap equal? serial-list (append (rest serial-list) (list null)))))))

#;(check-expect (no-duplicate-transactions? EX-BC-0) #true)
#;(check-expect (no-duplicate-transactions? EX-BC-1) #true)

;; all-signatures-ok? : Blockchain -> Boolean
;; Determines if every transaction in the blockchain has a valid signature.
(define (all-signatures-ok? bc)
  (andmap (lambda (x)
            (check-signature (transaction-sender-key x)
                             (string-append (transaction-unique-string x)
                                            (transaction-receiver-key x)
                                            ":"
                                            (number->string (transaction-amount x)))
                             (transaction-sender-sig x)))
          (map-transactions identity bc)))

#;(check-expect (all-signatures-ok? EX-BC-0) #true)
#;(check-expect (all-signatures-ok? EX-BC-1) #true)

;; account-balances-ok? : Blockchain -> Boolean
;; Determines if every transaction sends at least 1 accelcoin and every sender
;; always has enough accelcoin to send.
(define (account-balances-ok? bc)
  (not (false? (update-ledger/blockchain bc (make-hash '())))))

#;(check-expect (account-balances-ok? EX-BC-0) #true)
#;(check-expect (account-balances-ok? EX-BC-1) #false)

;; hash-update : [Hash-table-of X Y] X (Y -> Y) Y -> [Hash-table of X Y]
;; updates entry using function if present, else default
(define (hash-update h k upd def)
  (hash-set h k (if (hash-has-key? h k) (upd (hash-ref h k)) def)))

#;(check-expect (hash-update (make-hash (list)) "foo" add1 0) (make-hash (list (list "foo" 0))))
#;(check-expect (hash-update (make-hash (list (list "foo" 0) (list "bar" 0))) "foo" add1 0)
                (make-hash (list (list "foo" 1) (list "bar" 0))))

;; A Ledger is a [Hash-Table-of PublicKey Nat]
;; A ledger maps wallet IDs (public keys) to the number of accelcoins they have.

(define MINER-REWARD 100)
;; reward : PublicKey Ledger -> Ledger
;; Grants the miner the reward for mining a block.
(define (reward PubKey Ledger)
  (hash-update Ledger PubKey (lambda (x) (+ MINER-REWARD x)) MINER-REWARD))

#;(check-expect (reward ALICE-PUBLIC-KEY (make-hash '()))
                (make-hash (list (list ALICE-PUBLIC-KEY 100))))
#;(check-expect (reward ALICE-PUBLIC-KEY (make-hash (list (list ALICE-PUBLIC-KEY 1000))))
                (make-hash (list (list ALICE-PUBLIC-KEY 1100))))

;; update-ledger/transaction: Transaction Ledger -> [Optional Ledger]
;; Updates the ledger with a single transaction. Produces #false if
;; the sender does not have enough accelcoin to send.
(define (update-ledger/transaction tr l)
  (cond
    [(or (not (hash-has-key? l (transaction-sender-key tr)))
         (> (transaction-amount tr) (hash-ref l (transaction-sender-key tr)))
         (<= (transaction-amount tr) 0))
     #f]
    [else
     (hash-update (hash-update l
                               (transaction-receiver-key tr)
                               (lambda (x) (+ x (transaction-amount tr)))
                               (transaction-amount tr))
                  (transaction-sender-key tr)
                  (lambda (x) (- x (transaction-amount tr)))
                  0)]))

#;(check-expect (update-ledger/transaction EX-TRANSACTION-0 (make-hash '())) #false)
#;(check-expect
   (update-ledger/transaction EX-TRANSACTION-0 (make-hash (list (list ALICE-PUBLIC-KEY 1000))))
   (make-hash (list (list ALICE-PUBLIC-KEY 900) (list BOB-PUBLIC-KEY 100))))

;; update-ledger/block : Block Ledger -> [Optional Ledger]
;; Updates the ledger with the transactions in a block, and rewards the miner.
;; Produces #false if any transaction in the block would make a sender's
;; balance negative. The miner receives their reward *after* all transactions
;; are procsed.
(define (update-ledger/block b l)
  ;; ledge-helper : [List-of Transaction] Ledger -> [Optional Ledger]
  ;; Given list of transactions, updates ledger with each transaction from right to left
  (local ((define (ledge-helper lot ledge)
            (cond
              [(empty? lot) ledge]
              [else
               (local ((define prev (ledge-helper (rest lot) ledge)))
                 (if (boolean? prev) #f (update-ledger/transaction (first lot) prev)))]))
          (define lh (ledge-helper (reverse (block-transactions b)) l)))
    (if (boolean? lh) #f (reward (block-miner-key b) lh))))

;; update-ledger/blockchain : Blockchain Ledger -> [Optional Ledger]
;; Produces the ledger for a blockchain, or #false if any transaction
;; would make the sender's balance negative.
(define (update-ledger/blockchain bc ledge)
  (cond
    [(empty? bc) ledge]
    [else
     (local ((define prev (update-ledger/blockchain (rest bc) ledge)))
       (if (boolean? prev) #f (update-ledger/block (first bc) prev)))]))

#;(check-expect (update-ledger/blockchain EX-BC-0 (make-hash '()))
                (make-hash (list (list ALICE-PUBLIC-KEY 190) (list BOB-PUBLIC-KEY 10))))

;; =========================
;; NETWORK STUFF

;; send-transaction: PrivateKey PublicKey Nat -> Boolean
;; (send-transaction sender-private-key receiver-public-key amount) sends a
;; transactions to the Accelchain broadcaster.
(define (send-transaction priv pub amt)
  (post-data "accelchain.api.breq.dev"
             "/"
             (transaction->string2 (build-transaction 0 priv pub amt))))

;; A ValidatorState is a (make-valstate Ledger [List-of Hashtable] [List-of Hashtable] Number)
;; (make-valstate bc ledge transactions) represents a collection containing the
;; current blockchain, the ledger with all the account balances, and a list of
;; transactions that have already been validated.
(define-struct valstate [ledge pt ut prev-digest])

(define INIT-STATE
  (make-valstate
   (make-hash
    (list
     (list
      "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
      100)))
   (make-hash '())
   (make-hash '())
   0))

;; filter-transactions : ValidatorState -> Transaction
;; Creates a temporary ledger, and filters out all the transactions in the pending-transactions
;; that return false from update-ledger/transaction, while updating the temporary ledger.
(define (filter-transactions st)
  (local
    [(define txs (valstate-pt st))
     (define txs-values (hash-values txs))]
    (first (foldl
            (lambda (tx acc)
              (local [(define lst (first acc))
                      (define ledger (second acc))
                      (define updated-ledger (update-ledger/transaction tx ledger))]
                (if (false? updated-ledger)
                    acc
                    (list (cons tx lst) updated-ledger))))
            (list '() (valstate-ledge st))
            txs-values))))

;; update-ledger/transaction: Transaction Ledger -> [Optional Ledger]
;; handle-transaction : ValidatorState Transaction -> [Optional ValidatorState]
(define (handle-transaction vs tran)
  (if (and (not (false? (update-ledger/transaction tran (valstate-ledge vs))))
           (check-signature (transaction-sender-key tran)
                            (string-append (transaction-unique-string tran)
                                           (transaction-receiver-key tran)
                                           ":"
                                           (number->string (transaction-amount tran)))
                            (transaction-sender-sig tran))
           (not (member? (transaction-unique-string tran)
                         (map transaction-unique-string (hash-values (valstate-pt vs)))))
           (not (member? (transaction-unique-string tran)(hash-keys (valstate-ut vs)))))
      (make-valstate (valstate-ledge vs)
                     (hash-update (valstate-pt vs) (transaction-serial tran) (lambda (x) x) tran)
                     (valstate-ut vs)
                     (valstate-prev-digest vs))
      #f))


(check-expect (handle-transaction (handle-transaction INIT-STATE EX-TRANS-1) EX-TRANS-1) #f)
(check-expect (handle-transaction INIT-STATE EX-TRANS-1)
              (make-valstate (valstate-ledge INIT-STATE)
                             (make-hash (list (list (transaction-serial EX-TRANS-1) EX-TRANS-1)))
                             (make-hash '()) 0))

;; handle-block : ValidatorState Block -> [Optional ValidatorState]
;; Checks to make sure the block provided is valid.
(define (handle-block vs b)
  (local
    ;; check-transactions-handled : [List-of Transaction] -> Boolean
    ((define (check-transactions-handled l)
       (cond
         [(empty? l) #t]
         [else
          (and (member? (transaction-unique-string (first l))
                        (map transaction-unique-string (hash-values (valstate-pt vs))))
               (check-transactions-handled (rest l)))]))
     ;; remove-handled-transactions : [Hash-Table-of String Transaction] [List-of Transaction] -> [Hash-Table-of Transaction]
     (define (remove-handled-transactions hash l)
       (cond
         [(empty? (rest l)) (hash-remove hash (transaction-serial (first l)))]
         [else (remove-handled-transactions (hash-remove hash (transaction-serial (first l))) (rest l))]))
                          
     (define check-ledge-update (update-ledger/block b (valstate-ledge vs)))
     (define check-new-digest
       (block-digest
        (if (equal? (valstate-ut vs) (valstate-ut INIT-STATE)) 0 (valstate-prev-digest vs))
        b))
     ;; update-transaction : [Hash-Table-of String Boolean] [List-of Transaction] -> [Hash-Table-of String Boolean]
     (define (update-transaction hash l)
       (cond
         [(empty? (rest l)) (hash-update hash (transaction-unique-string (first l)) (lambda (x) x) #t)]
         [else
          (hash-update (update-transaction hash (rest l)) (transaction-unique-string (first l)) (lambda (x) x) #t)]))) 
    (cond
      [(and (< check-new-digest DIGEST-LIMIT)
            (>= (length (block-transactions b)) 3)
            (not (false? check-ledge-update))
            (andmap (lambda (x) (hash-has-key? (valstate-pt vs) x)) (map transaction-serial (block-transactions b))))
       (make-valstate check-ledge-update
                      (remove-handled-transactions (valstate-pt vs) (block-transactions b))
                      (update-transaction (valstate-ut vs) (hash-values (valstate-pt vs)))
                      check-new-digest)]
      [else #f])))

(check-expect (handle-block INIT-STATE EX-BLOCK-1) #f)
(check-expect
 (handle-block (handle-transaction
                (handle-transaction (handle-transaction INIT-STATE EX-TRANS-1) EX-TRANS-2)
                EX-TRANS-3)
               EX-BLOCK-1)
 (make-valstate
  (make-hash
   (list
    (list "AAAAB3NzaC1yc2EAAAADAQABAAAAQQC+lhbrraDIZWIRzSeFlEzIR1nADcR9tDL9pp+UEJskvg1qoftSCzUtQiRW+iibyWs+mEXatiuYWi76216LcED9" 100)
    (list "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF" 50)
    (list "AAAAB3NzaC1yc2EAAAADAQABAAAAQQD482hkZnSwjUJrmlSZ75Jshk5Xf4U0Y0qQjDlEcEYieKHlu+2bJRviqkoONYwX38mjMO3EPiOpYY72MEUJymV5" 50)))
  (make-hash '())
  (make-hash (list (list "pdQJqUZNR4UcOaL9Z8zLTeac4vJCZJRBlUQscIk6qkQ=" #true)
                   (list "L85uJZ66mPX5UdP/2RKwhrVDy8Chf/Gi6/B5kSaGSTc=" #true)
                   (list "eEmCb7QfYFW1WBBQqIC8e5R/e6wINNvB1Hj4njhTSvA=" #true)))
  1503196597653199320545503337806019318374753496982761351917541399288737046))

(define EX-BLOCK-0
  (make-block
   '()
   1337
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"))

(define EX-TRANS-1
  (make-transaction
   1
   "L85uJZ66mPX5UdP/2RKwhrVDy8Chf/Gi6/B5kSaGSTc="
   "EzJeFbFWNfQr9+N+HMAqBJvQP+LokP2l6ZbUGJVxJsQo788hZPQPPobNHUCZnzpC3qhlU3EHGCS46nAOArmX0g=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQD482hkZnSwjUJrmlSZ75Jshk5Xf4U0Y0qQjDlEcEYieKHlu+2bJRviqkoONYwX38mjMO3EPiOpYY72MEUJymV5"
   20))

(define EX-TRANS-2
  (make-transaction
   2
   "eEmCb7QfYFW1WBBQqIC8e5R/e6wINNvB1Hj4njhTSvA="
   "l7SSPqyatoQCDZ5JFgfXnboxeyX924jGie0qMPsRJt/mh3dhpyHsLO9GnNX26v47oc5oHZGUd1S6OXxoRO7o4A=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQD482hkZnSwjUJrmlSZ75Jshk5Xf4U0Y0qQjDlEcEYieKHlu+2bJRviqkoONYwX38mjMO3EPiOpYY72MEUJymV5"
   15))

(define EX-TRANS-3
  (make-transaction
   3
   "pdQJqUZNR4UcOaL9Z8zLTeac4vJCZJRBlUQscIk6qkQ="
   "pH2H4JrbDfUdvzT7m76am1LOAMPeWVDoz/Vk1K14cfCR1CwPMAjNG7d7TdX9nphrlk6KK03gudoY+/2wBibn0Q=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQD482hkZnSwjUJrmlSZ75Jshk5Xf4U0Y0qQjDlEcEYieKHlu+2bJRviqkoONYwX38mjMO3EPiOpYY72MEUJymV5"
   15))

(define EX-BLOCK-1
  (make-block
   (list EX-TRANS-1 EX-TRANS-2 EX-TRANS-3)
   1346064127
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQC+lhbrraDIZWIRzSeFlEzIR1nADcR9tDL9pp+UEJskvg1qoftSCzUtQiRW+iibyWs+mEXatiuYWi76216LcED9"))

(define EX-BLOCK-1-MINED-DIFFERENTLY
  (make-block
   (list EX-TRANS-1 EX-TRANS-2 EX-TRANS-3)
   1346064127
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"))

(define EX-BLOCK-3
  (mine-block
   0
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   (list EX-TRANS-1 EX-TRANS-2 EX-TRANS-3)
   1000000))
(define EX-TRANS-4
  (make-transaction
   5
   "jUc72ub/Qw5IJNDxdYVfeebXN7I+nj9t8jgzXZa0Q3s="
   "BY1zexA7Ei/hvtCDSoW/kYE19AXVpT6I3JmWximvltwIDz5o2gltgySmW5FUXOAcvaDs5LUB8ooZAAwd6MwySQ=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDMZy0CtUcgZDnQ1H8FuCiDI8ETWYy1SCGFmegA24C39O2utfAH2RS+CD87noWmpK6qhe2pk3LgO1UWGc3uZS7t"
   10))

(define EX-TRANS-5
  (make-transaction
   6
   "7FwiG9uK64gK/1DnpG0XpRBqK56WGhpRRWc3xnpG6iA="
   "HqSx3i/7rvVXgDOKx8FYP7LOK6mOQdNaul9EalJwkmZ7LAAx3hgP81M9z7+kkOC9fx5MqvCa+rLJPTEBJ3g0zg=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDMZy0CtUcgZDnQ1H8FuCiDI8ETWYy1SCGFmegA24C39O2utfAH2RS+CD87noWmpK6qhe2pk3LgO1UWGc3uZS7t"
   10))

(define EX-TRANS-6
  (make-transaction
   7
   "aP7US0h/NB3yMFTtcI44rDw3kOnhyHTZ9xY3tzLUs6g="
   "mDR27ReLgwd3hoPo94G6EzWxGfAK/bp9WxOkrcgNdzqEZtCFTTOWM+Vd9HPm/mjlLtftcuI+yWcU5JL0nUhBJA=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDMZy0CtUcgZDnQ1H8FuCiDI8ETWYy1SCGFmegA24C39O2utfAH2RS+CD87noWmpK6qhe2pk3LgO1UWGc3uZS7t"
   10))

(define EX-BLOCK-4
  (mine-block
   (block-digest 0 EX-BLOCK-3)
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   (list EX-TRANS-4 EX-TRANS-5 EX-TRANS-6)
   1000000))

(define EX-NEW-TRANS-1
  (make-transaction
   1
   "8A/drE8IiU1XysumSz2wOYhqbJLZo1xpjd1pSglQHKo="
   "HHm0y8/MpffZCCVUgyCs+fxrUQm6HoxvR1BCUVvrwbR8zLoxhpjIBu0zzGEd8u9Huq2MSSsRgUxIKFNNo0dSAw=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
   20))

(define EX-NEW-TRANS-2
  (make-transaction
   2
   "rxbTMWaB9eHEmr2CV01vL9rc/lu54qb5kkMYjwgTUxE="
   "FSnUKwlhVQN5w3ZE6icJ2bqQI3nD4IDVC87P3IUkxobCSPnuTRRyzDh8HNvOL94oJioL4f7BP8mkqf0horiqsQ=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   5))

(define EX-NEW-TRANS-3
  (make-transaction
   3
   "pi8P0Lagg+uk5uVRfqRPjylTFjcXyN+Iufb2s1OnWn8="
   "o7A+SMUFsFEDtiCX3yUEuVhY/bZcxayqbJhY6Hxip4f5L+vOVhElI3Q9o0maIcXMdCa4a8gEAWEZa7rnfLpXiw=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
   2))

(define EX-NEW-BLOCK-1
  (make-block
   (list EX-NEW-TRANS-1 EX-NEW-TRANS-2 EX-NEW-TRANS-3)
   2363342893
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"))

(define EX-NEW-BLOCK-1-MINED-DIFFERENTLY
  (mine-block
   0
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
   (list EX-NEW-TRANS-1 EX-NEW-TRANS-2 EX-NEW-TRANS-3)
   1000000))

;; go : ValidatorState -> Stuff
;; Checks the entire blockchain.
(define (go init-state)
  (blockchain-big-bang init-state [on-transaction handle-transaction] [on-block handle-block]))

;(block-digest 0 EX-BLOCK-1)
;(go INIT-STATE)

;; ================
;; Mining
(require racket/string)

;; block->string : Block -> String
;; Serializes a block into a string with the format.
(define (block->string blk)
  (local [(define transactions (block-transactions blk))
          (define transaction-strings
            (map (lambda (t) (string-replace (transaction->string t) ":" ";")) transactions))
          (define transaction-string (string-join transaction-strings ":"))]
    (format "block:~a:~a:~a" (block-nonce blk) (block-miner-key blk) transaction-string)))

;; mine+validate : ValidatorState PublicKey Number -> Boolean
;;
;; (mine+validate state miner-key retries)
;;
;; Uses mine-block (from Part 1) to mine the pending transactions in
;; the validator state.
;;
;; Produces #false if the retries are exhausted or if the number of pending
;; transactions is less than three.
;;
;; If mining succeeds, sends the serialized block using post-data and produces
;; #true.
;; mine-block : Digest PublicKey [List-of Transaction] Nat -> [Optional Block]
(define (mine+validate state miner-key retries)
  (local
    [(define filteredState (filter-transactions state))
     (define minedBlock (mine-block (valstate-prev-digest state) miner-key
                                    filteredState retries))]
    (cond
      [(< (length filteredState) 3) #f]
      [else
       (if (boolean? minedBlock)
           #f
           (post-data "accelchain.api.breq.dev" "/" (block->string minedBlock)))])))


;; go-miner : ValidatorState PublicId Number -> ValidatorState
;;
;; (go-miner state miner-key retries) mines the pending transactions in state
;; uses `go` to validate the current blockchain, and then recurs indefinitely.
(define (go-miner state miner-key retries)
  (local [(define _ (mine+validate state miner-key retries))
          (define s2 (go state))]
    (go-miner s2 miner-key retries)))

;;(go-miner INIT-STATE PUBLIC-KEY-1 2001)