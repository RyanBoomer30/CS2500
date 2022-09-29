;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW-2-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;; Bouncing Ball

(require 2htdp/image)
(require 2htdp/universe)

;; A ball is a (make-ball Number Number Number Number)
;; Interpretation: A ball with position (x, y) and velocity (vx, vy)
(define-struct ball (x y vx vy))

;; A ball-state is a (make-balls-state [List-of ball] Number Number)
;; Interpretation: A balls-state with a list of ball, width and height of the windows
(define-struct balls-state (ballObject width height))

;; ball-template : ball -> ?
(define (ball-template b)
  (... (ball-x b) ...
       (ball-y b) ...
       (ball-vx b) ...
       (ball-vy b) ...))

;; balls-state-template : [List-of ball] Number Number -> ?
(define (balls-state-template b)
  (... (ballObject b) ...
       (width b) ...
       (height b) ...))

;; initilizes the starting positions of the ball. Sets the width and the height of the screen
;; balls-state-initialize : ball Number Number -> balls-state
(define (balls-state-initialize ball width height)
  (make-balls-state ball width height))

;; creates a new ball state
;; new-balls-state :  ball [List-of Number] -> new-balls-state
(define (new-balls-state newBalls lst)
  (make-balls-state newBalls (balls-state-width lst) (balls-state-height lst)))

;; Creates new ball
;; new-ball : Number Number Number Number -> ball
(define (new-ball new-x new-y new-vx new-vy)
  (make-ball new-x new-y new-vx new-vy))

;; creates the ball
(define EX-STATIONARY-BALL (make-ball 100 200 0 0))
(define EX-BALL-MOVING-HORZ (make-ball 100 200 10 0))
(define EX-BALL-MOVING-VERT (make-ball 100 200 0 10))
(define EX-BALL-MOVING-DIAG (make-ball 100 200 10 10))

;; creates a list of ball
(define list-of-balls
  (list EX-STATIONARY-BALL EX-BALL-MOVING-HORZ EX-BALL-MOVING-VERT EX-BALL-MOVING-DIAG))

;; gives shape, size, and color of ball object
(define draw-ball (circle 5 "solid" "red"))

;; displays the animation
;; Uses big-bang to animate the ball moving around in a window of given dimensions
;; animate-balls : [List-of ball] Number Number-> WorldState (
(define (animate-balls lst width height)
  (big-bang (balls-state-initialize list-of-balls width height)
            [to-draw render]
            [on-tick check-border]))

;; checks if a ball hit the border
;; check-border : ball -> [List-of ball]
(define (check-border ball)
  (local ((define lst (balls-state-ballObject ball)))
         (cond
           [(empty? lst) '()]
           [(cons? lst)
            (new-balls-state (check-bounce lst (balls-state-width ball) (balls-state-height ball))
                             ball)])))[
                                       
;; check expects for check-border
(check-expect (check-border (balls-state-initialize list-of-balls 500 500))
              (make-balls-state (list (make-ball 100 200 0 0)
                                      (make-ball 110 200 10 0)
                                      (make-ball 100 210 0 10)
                                      (make-ball 110 210 10 10))
                                500
                                500))
(check-expect (check-border (balls-state-initialize list-of-balls 300 400))
              (make-balls-state (list (make-ball 100 200 0 0)
                                      (make-ball 110 200 10 0)
                                      (make-ball 100 210 0 10)
                                      (make-ball 110 210 10 10))
                                300
                                400))

;; checks if ball bounces off a border
;; check-bounce : [List of ball] Number Number -> [List of ball]
(define (check-bounce lst width height)
  (cond
    [(empty? lst) '()]
    [(cons? lst) (cons (bounce (first lst) width height) (check-bounce (rest lst) width height))]))

;; check expect for bounce
(check-expect (check-bounce list-of-balls 500 500)
              (list (make-ball 100 200 0 0)
                    (make-ball 110 200 10 0)
                    (make-ball 100 210 0 10)
                    (make-ball 110 210 10 10)))
(check-expect (check-bounce list-of-balls 300 400)
              (list (make-ball 100 200 0 0)
                    (make-ball 110 200 10 0)
                    (make-ball 100 210 0 10)
                    (make-ball 110 210 10 10)))

;; changes the direction of the ball if it hits the border
;; bounce : [List of ball] Number Number -> [List of ball]
(define (bounce lst width height)
  (cond
    [(and (>= (ball-x lst) 0) (>= (ball-y lst) 0))
     (cond
       [(> (+ (ball-x lst) (ball-vx lst)) width)
        (new-ball (ball-x lst) (ball-y lst) (* -1 (ball-vx lst)) (ball-vy lst))]
       [(> (+ (ball-y lst) (ball-vy lst)) height)
        (new-ball (ball-x lst) (ball-y lst) (ball-vx lst) (* -1 (ball-vy lst)))]
       [else
        (new-ball (+ (ball-x lst) (ball-vx lst))
                  (+ (ball-vy lst) (ball-y lst))
                  (ball-vx lst)
                  (ball-vy lst))])]
    [(and (<= (ball-x lst) width) (<= (ball-y lst) height))
     (cond
       [(< (+ (ball-x lst) (ball-vx lst)) 0)
        (new-ball (ball-x lst) (ball-y lst) (* -1 (ball-vx lst)) (ball-vy lst))]
       [(< (+ (ball-y lst) (ball-vy lst)) 0)
        (new-ball (ball-x lst) (ball-y lst) (ball-vx lst) (* -1 (ball-vy lst)))]
       [else
        (new-ball (+ (ball-x lst) (ball-vx lst))
                  (+ (ball-vy lst) (ball-y lst))
                  (ball-vx lst)
                  (ball-vy lst))])]))

;; check expects for bounce
(check-expect (bounce EX-BALL-MOVING-HORZ 200 200) (make-ball 110 200 10 0))
(check-expect (bounce EX-BALL-MOVING-VERT 200 200) (make-ball 100 200 0 -10))
(check-expect (bounce EX-BALL-MOVING-DIAG 200 200) (make-ball 100 200 10 -10))

;; draws a still image of our current world state
;; render : ball-state -> image
(define (render ball)
  (local ((define lst (balls-state-ballObject ball)))
         (cond
           [(empty? lst) (empty-scene (balls-state-width ball) (balls-state-height ball))]
           [(cons? lst)
            (place-image draw-ball
                         (abs (ball-x (first lst)))
                         (abs (ball-y (first lst)))
                         (render (new-balls-state (rest lst) ball)))])))

;; check-expect for render
(check-expect (render (balls-state-initialize (list EX-BALL-MOVING-DIAG) 200 400))
              (place-image (circle 5 "solid" "red") 100 200 (empty-scene 200 400)))
(check-expect (render (balls-state-initialize (list EX-BALL-MOVING-HORZ) 200 400))
              (place-image (circle 5 "solid" "red") 100 200 (empty-scene 200 400)))
(check-expect (render (balls-state-initialize (list EX-BALL-MOVING-VERT) 200 400))
              (place-image (circle 5 "solid" "red") 100 200 (empty-scene 200 400)))
(check-expect (render (balls-state-initialize (list EX-STATIONARY-BALL) 200 400))
              (place-image (circle 5 "solid" "red") 100 200 (empty-scene 200 400)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;List Processing Funcitons

;; A [List-of X] is one of
;; - '()
;; - (cons X [List-of X])
;; Interpretation: A list of elements of type X.

;; list-template : [List-of X] -> ?
(define (list-template alist)
  (cond
    [(empty? alist) ...]
    [(cons? alist) (... (first alist) (list-template (rest alist)) ...)]))

(define EX-STRING-LIST (cons "a" (cons "b" '())))
(define EX-NUM-LIST (cons 1 (cons 2 (cons 3 '()))))
(define EX-LIST-LIST (cons (cons 1 (cons 20 '())) (cons (cons 2 '()) '())))

;; take two list of numbers and produce a list of their alternating items
;; interleave : [List-of num] [List-of num] -> [List-of num]
(define (interleave lst1 lst2)
  (cond
    [(and (empty? lst1) (empty? lst2)) '()]
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (cons (first lst1) (cons (first lst2) (interleave (rest lst1) (rest lst2))))]))

(check-expect (interleave (list 1 3 5) (list 2 4 6)) (list 1 2 3 4 5 6))
(check-expect (interleave (list 1 4 8) (list 2 3)) (list 1 2 4 3 8))
(check-expect (interleave (list 1 3 5) (list 2 4 6 8 9 10)) (list 1 2 3 4 5 6 8 9 10))

;; create a list of number of range (2 to n)
;; prime-nums: Number -> [List-of number]
(define (num-list n)
  (cond
    [(> 2 n) '()]
    [else (range 2 (+ n 1) 1)]))

(check-expect (num-list 2) (list 2))
(check-expect (num-list 6) (list 2 3 4 5 6))
(check-expect (num-list 1) '())

;; iterate through the list and return a list of prime number
;; prime-iterate: [List-of Number] Number -> [List-of Number]
(define (prime-iterate lst n)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (if (= (modulo n (first lst)) 0)
         (cons (first lst) (prime-iterate lst (/ n (first lst))))
         (prime-iterate (rest lst) n))]))

(check-expect (prime-iterate (list 2 3 4 5 6) 6) (list 2 3))
(check-expect (prime-iterate (list 2 3 4 5 6 7) 7) (list 7))
(check-expect (prime-iterate (list 2 3 4 5 6 7 8 9 10) 10) (list 2 5))

;; returns all the prime factors of an inputed number
;; prime-factors : Number -> [List of Number]
(define (prime-factors n)
  (local ((define alist (num-list n))) (prime-iterate alist n)))

(check-expect (prime-factors 5) (list 5))
(check-expect (prime-factors 20) (list 2 2 5))
(check-expect (prime-factors 1) '())




;; return all the sublist of an item and a list
;; check-out-of-range : Number [List-of Number] -> [List-of Number] 
(define (check-out-of-range first second)
  (cond
    [(empty? second) (list first)]
    [(cons? second)
     (cons (append (list first) second)
           (check-out-of-range first (reverse (rest (reverse second)))))]))

(check-expect (check-out-of-range 1 (list 2 3 4 5))
              (list (list 1 2 3 4 5) (list 1 2 3 4) (list 1 2 3) (list 1 2) 1))
(check-expect (check-out-of-range 1 (list 2)) (list (list 1 2) 1))
(check-expect (check-out-of-range 1 (list 3 5 6 7))
              (list (list 1 3 5 6 7) (list 1 3 5 6) (list 1 3 5) (list 1 3) 1))

;; return all the sublist of a list
;; powerlist : [List-of Number] -> [List-of Number]
(define (powerlist lst)
  (cond
    [(empty? lst) '()]
    [(cons? lst) (cons (check-out-of-range (first lst) (rest lst)) (powerlist (rest lst)))]))

(check-expect (powerlist (list 1 2)) (list (list (list 1 2) 1) (list 2)))
(check-expect (powerlist (list 4 8 9))
              (list (list (list 4 8 9) (list 4 8) 4) (list (list 8 9) 8) (list 9)))
(check-expect (powerlist (list 3)) (list (list 3)))

;; given two lists, return any element from the first list that is contained in all the sublists of the second list
;; contained : [List-of Number] [Non-empty List Number] -> [List-of Number]
(define (contained firstlst restlst)
  (cond
    [(empty? firstlst) '()]
    [(cons? firstlst)
     (local [(define containedElement?
               (cond
                 [(numcontained (first firstlst) restlst) (first firstlst)]
                 [else '()]
                 ))]
       (cons containedElement? (contained (rest firstlst) restlst)))]))

(check-expect (contained (list 1 2 3) (list (list 5 2 3) (list 2 5 4) (list 5 6 2))) (list '() 2 '()))
(check-expect (contained (list 1 2 3) (list (list 1 2 3) (list 2 5 1) (list 5 1 2))) (list 1 2 '()))

;; given a number and a list, check if the number is included in every lists
;; numcontained : Number [Non-empty List Number] -> Boolean
(define (numcontained n lst)
  (local [(define (equalElement element)
            (member? n element)
            )]
  (andmap equalElement lst)))

(check-expect (numcontained 5 (list (list 5 2 3) (list 2 5 4) (list 5 6 2))) #true)
(check-expect (numcontained 6 (list (list 5 2 3) (list 2 5 4) (list 5 6 2))) #false)

;; given a list, check if it has an element that exists in all inner lists
;; intersection : [Non-empty List Number] -> [List-of Number]
(define (intersection lst)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (filter number? (contained (first lst) (rest lst)))
     ]
  ))

(check-expect (intersection (list (list 1 2 3) (list 2 3 4) (list 5 6 2))) (list 2))
(check-expect (intersection (list (list 1 2 3) (list 2 1 4) (list 5 1 2))) (list 1 2))
(check-expect (intersection (list (list 1 2 3) (list 4 5 6) (list 7 8 9))) '())

;;earliest [List-of String] helper-function -> String
(define (earliest lst f)
  (cond
    [(empty? lst) ""]
    [(empty? (rest lst)) (first lst)]
    [(f (first lst) (second lst)) (earliest (remove (second lst) lst) f)]
    [else (earliest (remove (first lst) lst) f)]))

;; returns the word with the earlist first letter
;; earliest-lexicographically : [List-of String] -> String
(define (earliest-lexicographically lst)
  (cond
    [(empty? lst) ""]
    [else (earliest lst string<?)]))

(check-expect (earliest-lexicographically (list "hello" "world" "zoo")) "hello")
(check-expect (earliest-lexicographically (list "zoo" "world" "hello")) "hello")
(check-expect (earliest-lexicographically (list "one" "two" "three")) "one")

;; returns the word with the latest first letter
;; last-lexicographically : [List-of String] -> String
(define (last-lexicographically lst)
  (cond
    [(empty? lst) ""]
    [else (earliest lst string>?)]))

(check-expect (last-lexicographically (list "hello" "world" "zoo")) "zoo")
(check-expect (last-lexicographically (list "zoo" "world" "hello")) "zoo")
(check-expect (last-lexicographically (list "one" "two" "three")) "two")

;; returns the last letter of a list of string 
;; [List-of String] -> String
(define (last-string lst)
  (cond
    [(empty? lst) ""]
    [else (earliest (list (first (reverse lst))) string=?)]))

(check-expect (last-string (list "hello" "world" "zoo")) "zoo")
(check-expect (last-string (list "zoo" "world" "hello")) "hello")
(check-expect (last-string (list "one" "two" "three")) "three")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Building Abstractions 1

;; A [List-of X] is one of:
;; - (cons X [List-of X])
;; - '()

;; hello-everyone: [List-of String] -> [List-of String]
;; Greets everyone in the list.
(define (hello-everyone los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (string-append "Hello, " (first los) "!") (hello-everyone (rest los)))]))

;; words-until-period: [List-of String] -> [List-of String]
;; Produces the words in the list up to the first period.
(define (words-until-period los)
  (hw2-ba1 is-period? los))

;; String -> Boolean
;; checks if the string inputed is a period or not
(define (is-period? s)
  (string=? s "."))

;; starting-positive-numbers : [List-of Number] -> [List-of Number]
;; Produces the prefix of positive numbers in the list.
(define (starting-positive-numbers lon)
  (hw2-ba1 negative? lon))

;; hw2-ba1 : function [List-of X] -> [List-of Y]  
(define (hw2-ba1 fun lox)
  (cond
    [(empty? lox) '()]
    [(cons? lox) (if (fun (first lox)) '() (cons (first lox) (hw2-ba1 fun (rest lox))))]))

(check-expect (starting-positive-numbers (list 10 20 3 45 -90 8 5)) (list 10 20 3 45))
(check-expect (starting-positive-numbers (list 10 -20 3 45 -90 8 5)) (list 10))
(check-expect (starting-positive-numbers (list -10 20 3 45 -90 8 5)) '())

(check-expect (words-until-period (list "hello" "." "hi")) (list "hello"))
(check-expect (words-until-period (list "hello" " there" "." "How")) (list "hello" " there"))
(check-expect (words-until-period (list "." "hi")) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Building Abstractions 2

;; A [List-of X] is one of:
;; - (cons X [List-of X])
;; - '()

;; hello-goodbye [List-of String] -> [List-of String]
;; takes an input and appends hello infront of it as well as goodbye on it and returns two outputs per input
(define (hello-goodbye alist)
  (hw2-ba2 alist hello bye))

;; hello : String -> String
;; appends hello to a string
(define (hello s)
  (string-append "Hello " s "!"))

;; bye : String -> String
;; appends Goodbye to a string
(define (bye s)
  (string-append "Goodbye " s "!"))

(check-expect (hello-goodbye '()) '())
(check-expect (hello-goodbye (cons "Alice" (cons "Bob" '())))
              (cons "Hello Alice!"
                    (cons "Goodbye Alice!" (cons "Hello Bob!" (cons "Goodbye Bob!" '())))))

;; double-double [List-of Numbers] -> [List-of Numbers]
;; Multiplies each interval by two and by four
(define (double-double lon)
  (hw2-ba2 lon times-2 times-4))

;; times-2 : Number -> Number
;; multiplies a number by two
(define (times-2 n)
  (* 2 n))

;; times-4 : Number -> Number
;; multiplies a number by four
(define (times-4 n)
  (* 4 n))

(check-expect (double-double '()) '())
(check-expect (double-double (cons 10 (cons 20 '()))) (cons 20 (cons 40 (cons 40 (cons 80 '())))))

;; string-length-length : [List of String] -> [List of Numbers]
;; returns the length of a string as well as half of the length of the string
(define (string-length-length los)
  (hw2-ba2 los string-length divide))

;; divide : String -> Number
;;Gets the length of a string and divides it by two
(define (divide s)
  (/ (string-length s) 2))

(check-expect (string-length-length '()) '())
(check-expect (string-length-length (cons "Hello" '())) (cons 5 (cons 2.5 '())))


;; hw2-ba2 : [List-of X] helper-function1 helper-function2 -> [List-of Y]  
(define (hw2-ba2 alist f1 f2)
  (cond
    [(empty? alist) '()]
    [(cons? alist) (cons (f1 (first alist)) (cons (f2 (first alist)) (hw2-ba2 (rest alist) f1 f2)))]))
