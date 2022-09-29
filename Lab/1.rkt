;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define (until-function f lon)
  (cond
    [(empty? lon) '()]
    [(f (first lon)) '()]
    [else (cons (first lon) (until-function f (rest lon)))]
  ))

(define (empty-string? str)
  (string=? str "")
  )

(check-expect (until-function negative? (list 1 2 4 -3 5)) (list 1 2 4))
(check-expect (until-function empty-string? (list "Hello" "Joe" "" "mama")) (list "Hello" "Joe"))

(define (are-all? f los)
     (cond
         [(empty? los) #t]
         [else (and (f (first los))
                    (are-all? f (rest los)))]))

 (check-expect (are-all? string-lower-case? (list "abc" "12a" "ui" "989")) #f)
 (check-expect (are-all? even? (list 1 15 -23)) #f)


 ;; -------------------
 (define (ormap? s)
  (cond
    [(circl? s)
     (string=? (circl-c s) "green")]
    [(squar? s)
     (string=? (squar-c s) "green")]
    [(rectangl? s)
     (string=? (rectangl-c s) "green")])) 

 

 (define-struct circl [radius mode c])
(define-struct squar [side-length mode c])
(define-struct rectangl [width height mode c])

(define CIRC (make-circl 5 "solid" "green"))
(define SQUARE (make-squar 10 "outline" "blue"))
(define RECT (make-rectangl 15 20 "solid" "red"))

(check-expect (ormap ormap? (list CIRC SQUARE RECT)) #true)

(check-expect (filter circl? (list CIRC SQUARE RECT)) (list CIRC))

 (define (make-shape s)
  (cond
    [(circl? s)
     (circle (circl-radius s)(circl-mode s)(circl-c s))]
    [(squar? s)
     (square (squar-side-length s)(squar-mode s)(squar-c s))]
    [(rectangl? s)
     (rectangle (rectangl-width s)(rectangl-height s)(rectangl-mode s)(rectangl-c s))]))

 (define (shape-color s)
  (cond
    [(circl? s)
     (circl-c s)]
    [(squar? s)
     (squar-c s)]
    [(rectangl? s)
     (rectangl-c s)])) 
 
(check-expect (map shape-color (list CIRC SQUARE RECT)) (list "green" "blue" "red"))

(foldr above empty-image (map make-shape(list CIRC SQUARE RECT)))