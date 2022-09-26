;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct ball (x y vx vy))
(define-struct balls-state (ballObject width height))
;; A Ball is a (make-ball Number Number Number Number)
;; Interpretation: A ball with position (x, y) and velocity (vx, vy)

;; ball-template : Ball -> ?
(define (ball-template b)
  (... (ball-x b) ... (ball-y b) ... (ball-vx b) ... (ball-vy b) ...))

(define (balls-state-initialize ball width height)
  (make-balls-state
  ball
  width height))

(define (new-balls-state newBalls lst)
  (make-balls-state
  newBalls
  (balls-state-width lst)
  (balls-state-height lst)))

(define (new-ball new-x new-y new-vx new-vy)
  (make-ball
  new-x
  new-y
  new-vx
  new-vy))

(define EX-STATIONARY-BALL (make-ball 100 200 0 0))
(define EX-BALL-MOVING-HORZ (make-ball 200 200 10 0))
(define EX-BALL-MOVING-VERT (make-ball 100 300 0 10))
(define EX-BALL-MOVING-DIAG (make-ball 200 300 10 10))

(define list-of-balls (list EX-STATIONARY-BALL EX-BALL-MOVING-HORZ EX-BALL-MOVING-VERT EX-BALL-MOVING-DIAG))

(define WIDTH 300)
(define HEIGHT 400)
(define BALL (circle 5 "solid" "red"))
(define MT   (empty-scene WIDTH HEIGHT))
 
(define (animate-balls lst width height)
  (big-bang (balls-state-initialize list-of-balls 500 200)
    [to-draw render]
    [on-tick check-border]
    ))
 
(define (check-border ball)
  (local ([define lst (balls-state-ballObject ball)])
  (cond
    [(empty? lst) '()]
    [(cons? lst)
      (new-balls-state (check-bounce lst) ball)
     ])))

(define (check-bounce lst)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (cons
      (bounce (first lst))
      (check-bounce (rest lst))
      )
     ])
 )

(define (bounce lst)
  (cond
    [(and (>= (ball-x lst) 0) (>= (ball-y lst) 0))
     (cond
       [(> (+ (ball-x lst) (ball-vx lst)) WIDTH) (new-ball (ball-x lst)(ball-y lst)
                                                                            (* -1 (ball-vx lst))(ball-vy lst))]
       [(> (+ (ball-y lst) (ball-vy lst)) HEIGHT) (new-ball (ball-x lst)(ball-y lst)(ball-vx lst)
                                                           (* -1 (ball-vy lst)))]
       [else (new-ball (+ (ball-x lst) (ball-vx lst))
                      (+ (ball-vy lst)(ball-y lst))(ball-vx lst)(ball-vy lst))])]
    [(and (<= (ball-x lst) WIDTH) (<= (ball-y lst) HEIGHT))
     (cond
       [(< (+ (ball-x lst) (ball-vx lst)) 0) (new-ball (ball-x lst)(ball-y lst)
                                                      (* -1 (ball-vx lst))(ball-vy lst))]
       [(< (+ (ball-y lst) (ball-vy lst)) 0) (new-ball (ball-x lst)(ball-y lst)
                                                      (ball-vx lst)(* -1 (ball-vy lst)))]
       [else (new-ball (+ (ball-x lst) (ball-vx lst))
                      (+ (ball-vy lst)(ball-y lst))(ball-vx lst)(ball-vy lst))])]
))

(define (render ball)
  (local ([define lst (balls-state-ballObject ball)])
   (cond
     [(empty? lst) MT]
     [(cons? lst)
      (place-image
       BALL (abs (ball-x (first lst))) (abs (ball-y (first lst))) (render (new-balls-state (rest lst) ball)))]
     )))
   
   
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
;; interleave : List-of num, List-of num -> List-of num
(define (interleave lst1 lst2)
  (cond
    [(and (empty? lst1) (empty? lst2)) '()]
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else
     (cons (first lst1) (cons (first lst2) (interleave (rest lst1) (rest lst2))))
     ]
))

(check-expect (interleave (list 1 3 5) (list 2 4 6)) (list 1 2 3 4 5 6))
(check-expect (interleave (list 1 4 8) (list 2 3)) (list 1 2 4 3 8))
(check-expect (interleave (list 1 3 5) (list 2 4 6 8 9 10)) (list 1 2 3 4 5 6 8 9 10))

;; create a list of number of range (2 to n)
;; prime-nums: int -> [List-of number]
(define (num-list n)
  (cond
    [(> 2 n) '()]
    [else (range 2 (+ n 1) 1)]
    )
  )

(check-expect (num-list 2) (list 2))
(check-expect (num-list 6) (list 2 3 4 5 6))
(check-expect (num-list 1) '())

;; iterate through the list and return a list of prime number
;; prime-iterate: [List-of Number] -> [List-of number] (n : number)
(define (prime-iterate lst n)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (if (= (modulo n (first lst)) 0)
         (cons (first lst) (prime-iterate lst (/ n (first lst))))
         (prime-iterate (rest lst) n))])
  )

(check-expect (prime-iterate (list 2 3 4 5 6) 6) (list 2 3))
(check-expect (prime-iterate (list 2 3 4 5 6 7) 7) (list 7))
(check-expect (prime-iterate (list 2 3 4 5 6 7 8 9 10) 10) (list 2 5))
  
;; returns all the prime factors of an inputed number
;; prime-factors : interger -> [List of Number]  
(define (prime-factors n)
  (local ([define alist (num-list n)])
    (prime-iterate alist n)
  ))

(check-expect (prime-factors 5) (list 5))
(check-expect (prime-factors 20) (list 2 2 5))
(check-expect (prime-factors 1) '())

;; return all the sublist of an item and a list
;; check-out-of-range : [List-of Number] -> [List-of Number] (first : number)
(define (check-out-of-range first second)
  (cond
    [(empty? second) (list first)]
    [(cons? second)
     (cons (append (list first) second) (check-out-of-range first (reverse (rest (reverse second)))))]
    )
)

(check-expect (check-out-of-range 1 (list 2 3 4 5)) (list (list 1 2 3 4 5) (list 1 2 3 4) (list 1 2 3) (list 1 2) 1))
(check-expect (check-out-of-range 1 (list 2)) (list (list 1 2) 1))
(check-expect (check-out-of-range 1 (list 3 5 6 7)) (list (list 1 3 5 6 7) (list 1 3 5 6) (list 1 3 5) (list 1 3) 1))

;; return all the sublist of a list
;; powerlist : [List-of Number] -> [List-of Number]
(define (powerlist lst)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (cons (check-out-of-range (first lst) (rest lst))
           (powerlist (rest lst)))
     ]
    )
)

(check-expect (powerlist (list 1 2)) (list (list (list 1 2) 1) (list 2)))
(check-expect (powerlist (list 4 8 9)) (list (list (list 4 8 9) (list 4 8) 4) (list (list 8 9) 8) (list 9)))
(check-expect (powerlist (list 3)) (list (list 3)))

;; check if a string is contained in an outer list
;; more-inner-check : [List-of String] -> string (string : string)
(define (more-inner-check string lst)
  (cond
    [(empty? lst) string]
    [(cons? lst)
     (if (member? string (first lst))
         (more-inner-check string (rest lst))
         ""
         )
     ]
    )
 )

(check-expect (more-inner-check "a" (list (list "a" "d" "c") (list "d" "b" "f") (list "d" "g" "o"))) "")
(check-expect (more-inner-check "d" (list (list "a" "d" "c") (list "d" "b" "f") (list "d" "g" "o"))) "d")

(define (inner-check first-lst lst)
  (cond
    [(empty? first-lst) ""]
    [(cons? first-lst)
     (if (not (string=? (more-inner-check (first first-lst) lst) ""))
         (more-inner-check (first first-lst) lst)
         (inner-check (rest first-lst) lst)
         )
     ]
    )
)

(check-expect (inner-check (list "a" "b" "c") (list (list "a" "d" "c") (list "b" "e" "f") (list "l" "g" "o"))) "")
(check-expect (inner-check (list "o" "b" "d") (list (list "a" "d" "c") (list "d" "b" "f") (list "d" "g" "o"))) "d")

(define (intersection lst)
  (cond
    [(empty? lst) ""]
    [(cons? lst)
     (if (string=? (inner-check (first lst) lst) "")
         (intersection (rest lst))
         (inner-check (first lst) lst)
         )
     ]
  ))

(check-expect (intersection (list (list "a" "b" "c") (list "d" "b" "f") (list "b" "g" "o"))) "b")
(check-expect (intersection (list (list "a" "d" "c") (list "d" "b" "f") (list "b" "g" "o"))) "")
(check-expect (intersection (list (list "a" "e" "c") (list "o" "a" "e") (list "e" "a" "o"))) "a")

(define (earliest lst f)
  (cond
    [(empty? lst) ""]
    [(empty? (rest lst)) (first lst)]
    [(f (first lst) (second lst)) (earliest (remove (second lst) lst)f)]
    [else (earliest (remove (first lst) lst)f)]
    )
)

(define (earliest-lexicographically lst)
  (cond
    [(empty? lst) ""]
    [else
     (earliest lst string<?)
     ]
    )
  )

(check-expect (earliest-lexicographically (list "hello" "world" "zoo")) "hello")
(check-expect (earliest-lexicographically (list "zoo" "world" "hello")) "hello")
(check-expect (earliest-lexicographically (list "one" "two" "three")) "one")

(define (last-lexicographically lst)
  (cond
    [(empty? lst) ""]
    [else
     (earliest lst string>?)
     ]
    )
  )

(check-expect (last-lexicographically (list "hello" "world" "zoo")) "zoo")
(check-expect (last-lexicographically (list "zoo" "world" "hello")) "zoo")
(check-expect (last-lexicographically (list "one" "two" "three")) "two")

(define (last-string lst)
  (cond
    [(empty? lst) ""]
    [else
     (earliest (list (first (reverse lst))) string=?)
     ]
    )
  )

(check-expect (last-string (list "hello" "world" "zoo")) "zoo")
(check-expect (last-string (list "zoo" "world" "hello")) "hello")
(check-expect (last-string (list "one" "two" "three")) "three")


(define (hw2-ba2 lst f v1 v2 v3 v4)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (cons (f v1 (first lst) v3)
     (cons (f v2 (first lst) v4)
           (hw2-ba2 (rest lst) f v1 v2 v3 v4)))
     ])
  )

(define (hello-goodbye lst)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (cons (string-append "Hello " (first lst) "!")
     (cons (string-append "Goodbye " (first lst) "!")
           (hello-goodbye (rest lst))))
     ]))

(check-expect (hello-goodbye '()) '())
(check-expect
  (hello-goodbye (cons "Alice" (cons "Bob" '())))
  (cons "Hello Alice!" (cons "Goodbye Alice!" (cons "Hello Bob!" (cons "Goodbye Bob!" '())))))

(define (double-double lst)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (cons (* 2 (first lst))
     (cons (* 4 (first lst))
           (double-double (rest lst))))
     ]))

(check-expect (double-double '()) '())
(check-expect (double-double (cons 10 (cons 20 '())))
              (cons 20 (cons 40 (cons 40 (cons 80 '())))))

(define (string-length-length lst)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (cons (string-length (first lst))
           (cons (/ (string-length (first lst)) 2)
                 (string-length-length (rest lst)))) 
     ]))

(check-expect (string-length-length '()) '())
(check-expect (string-length-length (cons "Hello" '()))
              (cons 5 (cons 2.5 '())))