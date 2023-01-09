;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; part 1

;; take-n : Int [List-of X] -> [List-of X]
;; (take-n n alist) produces the first n elements of the list in the order they appear.
;; n must be non-negative, and it produces the whole list if n exceeds the length of the
;; list.

(define (take-n n lst)
  (cond 
    [(>= n (length lst)) lst]
    [(empty? lst) '()]
    [(= n 0) '()]
    [(cons? lst)
     (cons (first lst) (take-n (- n 1) (rest lst)))]))

(check-expect (take-n 4 (list 0 1 2 3 4 5)) (list 0 1 2 3))
(check-expect (take-n 10 (list 1 2 3 4 5 6 7 8)) (list 1 2 3 4 5 6 7 8))
(check-expect (take-n 2 '()) '())


;; drop-n : Int [List-of X] -> [List-of X]
;; (drop-n n alist) produces alist but with the first n elements removed.
;; n must be non-negative, and drop-n produces the empty list if n exceeds the length of the
;; list.

(define (drop-n n lst)
  (cond
    [(> n (length lst)) '()]
    [(empty? lst) '()]
    [(cons? lst)
     (if (> n 0)
         (drop-n (- n 1) (rest lst))
         lst
         )
     ]))

(check-expect (drop-n 3 (list 1 2 3 4 5)) (list 4 5))
(check-expect (drop-n 0 (list 1 2 3 4 5)) (list 1 2 3 4 5))
(check-expect (drop-n 3 (list 1 2)) '())

;; take-while : (X -> Bool) [List-of X] -> [List-of X]
;; (take-while pred alist) produces the longest prefix of alist, where all
;; elements satisfy pred.

(define (take-while pred alist)
  (cond
    [(empty? alist) '()]
    [(cons? alist)
     (if (pred (first alist))
         (cons (first alist) (take-while pred (rest alist)))
         '()
         )
     ]
    )
  )
(check-expect (take-while positive? (list 1 2 3 4 5 -1 6 7 8 9 10)) (list 1 2 3 4 5))
(check-expect (take-while string? (list "a" "b" "c" "d" 1 "e" "f")) (list "a" "b" "c" "d"))
(check-expect (take-while positive? '())'())

;; drop-while : (X -> Bool) [List-of X] -> [List-of X]
;; (drop-while pred alist) produces a suffix of alist that we get by dropping
;; the longest prefix of alist that satisfies pred.

(define (drop-while pred alist)
  (cond
    [(empty? alist) '()]
    [(cons? alist)
     (if (pred (first alist))
         (drop-while pred (rest alist))
         (rest alist)
         )
     ]
    )
  )

(check-expect (drop-while positive? (list 1 2 3 4 5 -1 6 7 8 9 10)) (list 6 7 8 9 10))
(check-expect (drop-while string? (list "a" "b" "c" "d" 1 "e" "f")) (list "e" "f"))
(check-expect (drop-while positive? '())'())

;; group-by : (X X -> Boolean) [List-of X] -> [List-of [NE-List-of X]]
;; (group-by same-group? alist) splits alist into groups by comparing
;; consecutive elements to check if they should be in the same group.
(define (group-by same-group? alist)
  (local [;; same-group-list : [List-of X] -> [List-of Y]
          ;; return a list of consecutive elements in the same group
          (define (same-group-list alist)
            (cond
              [(empty? (rest alist)) alist]
              [(cons? alist)
               (if (same-group? (first alist) (first (rest alist)))
                   (cons (first alist) (same-group-list (rest alist)))
                   (list (first alist)))]))]
    (cond
      [(empty? alist) '()]
      [(empty? (rest alist)) (list alist)]
      [(cons? alist)
       (cons
        (same-group-list alist)
        (group-by same-group? (drop-n (length (same-group-list alist)) alist))
        )
       ])))

(check-expect (group-by = (list 10 10 20 20 20 10))
              (list (list 10 10) (list 20 20 20) (list 10)))
(check-expect (group-by < (list 20 30 40 30 50 20 30))
              (list (list 20 30 40) (list 30 50) (list 20 30)))