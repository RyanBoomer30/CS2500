;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (biggest-transformation-list l)
  (lambda (n) (apply max (map (lambda (x) (x n)) l )))
  )

(define plus8times4sqr
  (biggest-transformation-list
   (list
    (lambda (x) (+ x 8))
    (lambda (x) (* x 4))
    sqr)))

(define (sqr+1 lon)
  (map (λ (num) (+ 1 (sqr num))) lon))

#| Design the function two that takes a function f : [X -> X] as input and returns another
function that applies f twice in a row. That is, two returns a function which first applies
f to its input, then applies f again to the output of the first application (all within one function call). |#

(define (two f)
  (lambda (x) (f (f x))))

(define (three f)
  (lambda (x) (f (f (f x)))))

(define (one f)
  (lambda (x) (f x)))

(define (zero f)
  (lambda (x) x))

; A Repeater is a function [[X -> X] -> [X -> X]]
; That, given a one-argument function f, outputs a
; function that will repeatedly apply f some specific number of times
(define (repeater f)
  (local [(define (foldr-alt lst)
            (cond
              [(empty? lst) 0]
              [(empty? (rest lst)) (f (first lst))]
              [(cons? lst)
               (f (foldr-alt (rest lst)))
               ]
              )
            )]
    (lambda (x y) (foldr-alt (build-list x (lambda (z) y))))))

(check-expect ((repeater add1) 5 0) 5)

(define (rep->nat rep)
  ((repeater add1) ((rep add1) 0) 0))

(check-expect (rep->nat zero) 0)
(check-expect (rep->nat one) 1)
(check-expect (rep->nat two) 2)
(check-expect (rep->nat three) 3)
(check-expect (rep->nat (λ (f) (λ (x) ((three f) ((two f) x))))) 5)

(define (rep->add1 rep)
  (lambda ( x y) (one (rep x y))))