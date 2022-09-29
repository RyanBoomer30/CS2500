#lang slideshow
(require 2htdp/image)
(require dyoo-while-loop)

(define (score-by-length List-of-strings)
 (cond
   [(empty? List-of-strings) 0]
  [else (+ (string-length (first List-of-strings)) (score-by-length (rest List-of-strings)))])
)

(define (overlay-all List-of-images)
 (cond
   [(empty? List-of-images) (rectangle 10 10 "solid" "white")]
   [else
          (overlay (first List-of-images) (overlay-all (rest List-of-images)))])
  )


(define (bar-graph List-of-numbers)
 (cond
   [(empty? List-of-numbers) (rectangle 1 1 "solid" "white")]
   [else
          (beside/align "bottom" (rectangle 10 (first List-of-numbers) "solid" "black") (bar-graph (rest List-of-numbers)))
   ]
))

(define (is-in Any List-of-any)
  (define sortedList (sort List-of-any <))
  (define l 0)
  (define r (- (length List-of-any) 1))
  (let/ec return
  (while #true
         (when(> l r)
           (return #f)
         )
         (define m (floor (+ l (/ (- r l) 2))))
         (cond
           [(equal? (list-ref sortedList m) Any) (return #t)]
         )
         (if (< (list-ref sortedList m) Any) (set! l (+ m 1)) (set! r (- m 1)))
  ))
)

(define (words-to-sentence List-of-strings)
 (cond
   [(empty? (rest List-of-strings)) (first List-of-strings)]
   [else
      (string-append (string-append (first List-of-strings) " ") (words-to-sentence(rest List-of-strings)))    
   ]
))