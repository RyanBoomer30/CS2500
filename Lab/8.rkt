;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./hashtable-extras-2.rkt")
;; An Integer is one of:
;; - 0
;; - (add1 Integer)

;; my-gcd: Integer Integer -> Integer
(define (my-gcd x y)
  (cond
    [(= y 0) (list x y)]
    [else (my-gcd y (remainder x y))]))


(check-expect (fib 1) 1)
(check-expect (fib 2) 1)
(check-expect (fib 3) 2)
(check-expect (fib 35) 9227465) ;; quite slow!

(define (fib n)
  (local [(define (helper prev current new-n)
            (cond
              [(= new-n 1) prev]
              [else
               (helper current (+ prev current) (- new-n 1))]))]
    (helper 1 1 n)))

;; a Trie is [Hash-table-of 1String Trie]
;; The key for the Trie is one of:
;; "a" to "z", representing the letters
;; "" (the empty string), representing the termination marker.
;; The value represents the sub-trie (also called child) of the current Trie. 
;; The corresponding value for key "" should be an empty Trie (i.e. empty hash table).

;; empty trie is the base case. i.e. no strings are represented in trie.
(define empty-trie (make-hash empty))
;; This is a Trie that contains only the empty string ("").
(define empty-string-trie (make-hash (list (list "" empty-trie))))
;; A trie containing strings "cat" and "car".
(define TRIE-1
  (make-hash (list (list "c"
                         (make-hash
                          (list (list "a" (make-hash
                                           (list (list "t" empty-string-trie)
                                                 (list "r" empty-string-trie))))))))))
;; A trie containing strings "at" and "ate".
(define TRIE-2
  (make-hash (list (list "a"
                         (make-hash
                          (list (list "t" (make-hash
                                           (list (list "" empty-trie)
                                                 (list "e" empty-string-trie))))))))))