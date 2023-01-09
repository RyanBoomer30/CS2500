;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname CS2500_HW8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define PRIVATE-KEY-1 "MIIBOwIBAAJBAK59eNiP8RRwaa0y34piNlvYOyFDXtRf8NtneIoL8JMMvfVfH6X9a/zzIxHKxIlcY7WZCMVCN7+yU2L0uFE8zT8CAwEAAQJAR3HUk0eMvIOyoVodMXo5K64DuADvySFna06Yil2NKYf2iG6wttj5I2LoPHnK2EfaGGXelt1NBBCTobiJvlGfgQIhAOO22GUjOzNeG2iJO+DsUOijB5poPYIuP3sxBXPJLIpfAiEAxCojCtL0vvExVUylVYC9/f+8d37s0meNXnXIT5cCaSECIQCTJh7kDEY/RXrUrInjKRKHooJRRzcoHparFtx9G+0KZQIgO2+M+I2cjHImAXNcsQB2EIy4/PffJ40qn1FOpRbbKyECIQC9GErNEnR1dWL66hlxqDr67drQghpBgPkcKGYWW4qY7Q==")

(define PUBLIC-KEY-1 (secret->public PRIVATE-KEY-1))

;; build-transaction: Nat PrivateKey PublicKey Nat -> Transaction
;; (build-transaction serial sender-private-key receiver-public-key amount) builds a transaction
;; that sends amount from the sender to the receiver.
(define (build-transaction serial priv pub amt)
  (local
    ([define str (unique-string)])
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
;; "transaction:unique-string:sender-sig:sender-key:receiver-key,amount"
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

;; A genesis block where Alice starts the blockchain and receives the first mining reward.
(define EX-BLOCK-0
  (make-block '()
              8631727707325622792404128232286945630015639849891523695238049493932286431978
              ALICE-PUBLIC-KEY))


;; -------------Blockchain validation------------------------
;; blocks-sized-ok? : Blockchain -> Boolean
;; Determines that every block has at least three transactions, and that
;; the genesis block has zero transactions.
(define (blocks-sized-ok? bc)
  (and (empty? (block-transactions (first (reverse bc))))
       (andmap (lambda (x) (>= (length (block-transactions x)) 3)) (rest (reverse bc)))))

;; map-transactions : (Transaction -> X) Blockchain -> [List-of X]
;; Transaction-level map for blockchains.
(define (map-transactions f bc)
  (foldr (lambda (x y) (append (map f (block-transactions x)) y)) empty bc))


;; no-duplicate-transactions? : Blockchain -> Boolean
;; Determines if every transaction in the blockchain appears exactly once. Every
;; transaction has a unique serial number that we use to determine if it is unique.
(define (no-duplicate-transactions? bc)
  (local ((define serial-list (sort (map-transactions transaction-serial bc) <)))
    (not (ormap equal? serial-list (append (rest serial-list) (list null))))))


;; all-signatures-ok? : Blockchain -> Boolean
;; Determines if every transaction in the blockchain has a valid signature.
(define (all-signatures-ok? bc)
  (andmap (lambda (x)
            (check-signature
             (transaction-sender-key x)
             (string-append
              (transaction-unique-string x)
              (transaction-receiver-key x)
              ":"
              (number->string (transaction-amount x)))
             (transaction-sender-sig x)))
          (map-transactions identity bc)))


;; valid-digests? : Blockchain -> Boolean
;; Determines if every block has a valid digest.
(define (valid-digests? Blockchain)
  (local [;; valid-digests-helper : Blockchain -> [Optional Digest]
          ;; (valid-digests-helper bc) produces #false if any block has an invalid digest.]
          (define (valid-digests-helper bc)
            (cond
              [(empty? (rest bc)) 0]
              [else
               (if (boolean? (valid-digests-helper (rest bc)))
                   #false
                   (if (< (block-digest (valid-digests-helper (rest bc)) (first bc)) DIGEST-LIMIT)
                       (valid-digests-helper (rest bc))
                       #false))]))]
    (not (boolean? (valid-digests-helper Blockchain)))))


;; account-balances-ok? : Blockchain -> Boolean
;; Determines if every transaction sends at least 1 accelcoin and every sender
;; always has enough accelcoin to send.
(define (account-balances-ok? bc)
  (not (false? (update-ledger/blockchain bc (make-hash '())))))


;; hash-update : [Hash-table-of X Y] X (Y -> Y) Y -> [Hash-table of X Y]
;; updates entry using function if present, else default
(define (hash-update h k upd def)
  (hash-set h k (if (hash-has-key? h k) (upd (hash-ref h k)) def)))



;; A Ledger is a [Hash-Table-of PublicKey Nat]
;; A ledger maps wallet IDs (public keys) to the number of accelcoins they have.

(define MINER-REWARD 100)
;; reward : PublicKey Ledger -> Ledger
;; Grants the miner the reward for mining a block.
(define (reward PubKey Ledger)
  (hash-update Ledger PubKey (lambda (x) (+ MINER-REWARD x)) MINER-REWARD))



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



;; valid-blockchain? : Blockchain -> Boolean
;; Determines if a blockchain is valid.
(define (valid-blockchain? bc)
  (and (blocks-sized-ok? bc)
       (no-duplicate-transactions? bc)
       (all-signatures-ok? bc)
       (valid-digests? bc)
       (account-balances-ok? bc)))



;; =========================
;; NETWORK STUFF

;; send-transaction: PrivateKey PublicKey Nat -> Boolean
;; (send-transaction sender-private-key receiver-public-key amount) sends a
;; transactions to the Accelchain broadcaster.
(define (send-transaction priv pub amt)
  (post-data
   "broadcaster.federico.codes"
   "/"
   (transaction->string (build-transaction 0 priv pub amt))))

;; ValidatorState is a list of blockchain, list of transaction, ledger, prevDigest

;; handle-transaction : ValidatorState Transaction -> [Optional ValidatorState]
(define (handle-transaction vs tran)
  (local [(define list-unique-strings (map-transactions transaction-unique-string (first vs)))]
    (if (and
         (check-signature
          (transaction-sender-key tran)
          (string-append
           (transaction-unique-string tran)
           (transaction-receiver-key tran)
           ":"
           (number->string (transaction-amount tran)))
          (transaction-sender-sig tran))
         (not (member? tran (second vs)))
         )
        (list (first vs)
              (cons tran (second vs))
              (third vs)
              (fourth vs)
              )
        #false
        )))

(define miner-key "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF")
;; handle-block : ValidatorState Block -> [Optional ValidatorState]
(define (handle-block vs b)
  (local
    [(define minedBlockDigest (block-digest (fourth vs) b))]
    (if (and
         (andmap (lambda (x) (member? x (map transaction-unique-string (second vs)))) (map transaction-unique-string (block-transactions b))) 
         (< minedBlockDigest DIGEST-LIMIT)
         (<= 3 (length (block-transactions b)))
         (not (boolean? (update-ledger/block b (third vs))))
         ;; (andmap (lambda (x) (not (boolean? (handle-transaction vs x)))) (block-transactions b))
         )
        (list (cons b (first vs))
              '()
              (update-ledger/block b (third vs))
              minedBlockDigest)
        #false)
    ))

(define transaction1 (make-transaction
                      1
                      "8A/drE8IiU1XysumSz2wOYhqbJLZo1xpjd1pSglQHKo="
                      "HHm0y8/MpffZCCVUgyCs+fxrUQm6HoxvR1BCUVvrwbR8zLoxhpjIBu0zzGEd8u9Huq2MSSsRgUxIKFNNo0dSAw=="
                      "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
                      "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
                      20))

(define transactions (list
                      (make-transaction
                       3
                       "pi8P0Lagg+uk5uVRfqRPjylTFjcXyN+Iufb2s1OnWn8="
                       "o7A+SMUFsFEDtiCX3yUEuVhY/bZcxayqbJhY6Hxip4f5L+vOVhElI3Q9o0maIcXMdCa4a8gEAWEZa7rnfLpXiw=="
                       "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
                       "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
                       2)
                      (make-transaction
                       2
                       "rxbTMWaB9eHEmr2CV01vL9rc/lu54qb5kkMYjwgTUxE="
                       "FSnUKwlhVQN5w3ZE6icJ2bqQI3nD4IDVC87P3IUkxobCSPnuTRRyzDh8HNvOL94oJioL4f7BP8mkqf0horiqsQ=="
                       "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
                       "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
                       5)
                      (make-transaction
                       1
                       "8A/drE8IiU1XysumSz2wOYhqbJLZo1xpjd1pSglQHKo="
                       "HHm0y8/MpffZCCVUgyCs+fxrUQm6HoxvR1BCUVvrwbR8zLoxhpjIBu0zzGEd8u9Huq2MSSsRgUxIKFNNo0dSAw=="
                       "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
                       "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
                       20)))


(define init-state
  (list (list (make-block '() 1337 "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"))
        '()
        (make-hash (list(list "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF" 100)))
        0
        ))

(define (go init-state)
  (blockchain-big-bang
   init-state
   [on-transaction handle-transaction]
   [on-block handle-block]))

(define EX-TRANS-3
  (make-transaction
   3
   "pi8P0Lagg+uk5uVRfqRPjylTFjcXyN+Iufb2s1OnWn8="
   "o7A+SMUFsFEDtiCX3yUEuVhY/bZcxayqbJhY6Hxip4f5L+vOVhElI3Q9o0maIcXMdCa4a8gEAWEZa7rnfLpXiw=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
   2))

(define EX-TRANS-2
  (make-transaction
   2
   "rxbTMWaB9eHEmr2CV01vL9rc/lu54qb5kkMYjwgTUxE="
   "FSnUKwlhVQN5w3ZE6icJ2bqQI3nD4IDVC87P3IUkxobCSPnuTRRyzDh8HNvOL94oJioL4f7BP8mkqf0horiqsQ=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   5))

(define EX-TRANS-1
  (make-transaction
   1
   "8A/drE8IiU1XysumSz2wOYhqbJLZo1xpjd1pSglQHKo="
   "HHm0y8/MpffZCCVUgyCs+fxrUQm6HoxvR1BCUVvrwbR8zLoxhpjIBu0zzGEd8u9Huq2MSSsRgUxIKFNNo0dSAw=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDiztKZ2naYGUo6pZZqvUV9d1RErhDELuBqzxseh01spncM4avekEbkStHQBqYiW3/JQaBxCTKkMF6BMCSZsT7N"
   20))

(define EX-BLOCK-1
  (make-block
   (list EX-TRANS-3 EX-TRANS-2 EX-TRANS-1)
   1346064127
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQC+lhbrraDIZWIRzSeFlEzIR1nADcR9tDL9pp+UEJskvg1qoftSCzUtQiRW+iibyWs+mEXatiuYWi76216LcED9"))

(define EX-BLOCK-1-MINED-DIFFERENTLY
  (make-block
   (list EX-TRANS-3 EX-TRANS-2 EX-TRANS-1)
   1346064127
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"))

(define EX-BLOCK-3
  (mine-block 0
              "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
              (list EX-TRANS-1 EX-TRANS-2 EX-TRANS-3)
              1000000))

(define EX-TRANS-4
  (make-transaction
   4
   "jUc72ub/Qw5IJNDxdYVfeebXN7I+nj9t8jgzXZa0Q3s="
   "BY1zexA7Ei/hvtCDSoW/kYE19AXVpT6I3JmWximvltwIDz5o2gltgySmW5FUXOAcvaDs5LUB8ooZAAwd6MwySQ=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDMZy0CtUcgZDnQ1H8FuCiDI8ETWYy1SCGFmegA24C39O2utfAH2RS+CD87noWmpK6qhe2pk3LgO1UWGc3uZS7t"
   10))

(define EX-TRANS-5
  (make-transaction
   5
   "7FwiG9uK64gK/1DnpG0XpRBqK56WGhpRRWc3xnpG6iA="
   "HqSx3i/7rvVXgDOKx8FYP7LOK6mOQdNaul9EalJwkmZ7LAAx3hgP81M9z7+kkOC9fx5MqvCa+rLJPTEBJ3g0zg=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDMZy0CtUcgZDnQ1H8FuCiDI8ETWYy1SCGFmegA24C39O2utfAH2RS+CD87noWmpK6qhe2pk3LgO1UWGc3uZS7t"
   10))

(define EX-TRANS-6
  (make-transaction
   6
   "aP7US0h/NB3yMFTtcI44rDw3kOnhyHTZ9xY3tzLUs6g="
   "mDR27ReLgwd3hoPo94G6EzWxGfAK/bp9WxOkrcgNdzqEZtCFTTOWM+Vd9HPm/mjlLtftcuI+yWcU5JL0nUhBJA=="
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
   "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDMZy0CtUcgZDnQ1H8FuCiDI8ETWYy1SCGFmegA24C39O2utfAH2RS+CD87noWmpK6qhe2pk3LgO1UWGc3uZS7t"
   10))

(define EX-BLOCK-4
  (mine-block (block-digest 0 EX-BLOCK-3)
              "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF"
              (list EX-TRANS-4 EX-TRANS-5 EX-TRANS-6)
              1000000))

(handle-block (handle-transaction (handle-transaction (handle-transaction init-state EX-TRANS-1) EX-TRANS-2) EX-TRANS-3) EX-BLOCK-3)
(handle-block (handle-transaction (handle-transaction (handle-transaction (handle-block (handle-transaction
                                                                                         (handle-transaction (handle-transaction init-state EX-TRANS-1) EX-TRANS-2) EX-TRANS-3) EX-BLOCK-3) EX-TRANS-4) EX-TRANS-5) EX-TRANS-6) EX-BLOCK-4)