;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; shapeFunction: String -> String
;; Template

(define (shapeFunction s)
  (cond
    [(string=? s "circle") "..."]
    [(string=? s "square") "..."]
    [(string=? s "triangle") "..."]
    [else "error not a shape lmao"]
    )
  )

(check-expect (shapeFunction "circle") "...")
(check-expect (shapeFunction "rectangle") "error not a shape lmao")

;; draw-scene: String -> Image
;; Input a string of shape name and output an image

(define (draw-scene s)
  (cond
    [(string=? s "circle") (overlay (circle 20 "solid" "red")(square 100 "solid" "white"))]
    [(string=? s "square") (overlay (square 20 "solid" "red")(square 100 "solid" "white"))]
    [(string=? s "triangle") (overlay (triangle 20 "solid" "tan")(square 100 "solid" "white"))]
    [else "error not a shape lmao"]
    )
  )

(check-expect (draw-scene "circle") (overlay (circle 20 "solid" "red")(square 100 "solid" "white")))
(check-expect (draw-scene "square") (overlay (square 20 "solid" "red")(square 100 "solid" "white")))
(check-expect (draw-scene "triangle") (overlay (triangle 20 "solid" "tan")(square 100 "solid" "white")))
(check-expect (draw-scene "rectangle") (overlay (triangle 20 "solid" "tan")(square 100 "solid" "white")))

(require 2htdp/universe)

(define MOON (circle 20 "solid" "grey"))
(define SUN (circle 20 "solid" "yellow"))
(define SKY (square 200 "solid" "blue"))
(define NIGHT-SKY (square 200 "solid" "black"))

(define (place-moon-v3 x)
  (place-image
   MOON
   x 100
   (overlay SUN (cond
                  [(and (>= x 80) (<= x 120)) NIGHT-SKY]
                  [else SKY]))))

;; reset-position: Number KeyEvent -> Number
(define (reset-position x key)
  (if (string=? key "r")
      -100 x)
  )

#|(big-bang -100
  [to-draw place-moon-v3]
  [on-tick add1]
  [on-key reset-position]
  )|#

;; given a KeyEvent, return the door state
;; doorAction: KeyEvent(string) -> String


;; states: opened, closed, locked
;; returns: Can't open the door, Can't close the door, Can't lock the door, Can't unlock the door

;; doorState:String
(define doorState "opened")

;; doorAction: KeyEvent -> String
(define (doorAction key)
  (cond
    [(string=? key "o")
     (cond
       [(or (string=? doorState "locked") (string=? doorState "opened")) "Can't open the door"]
       [else "Open the door" (define doorState "opened") ])]
    [(string=? key "c")
     (cond
       [(or (string=? doorState "locked") (string=? doorState "closed")) "Can't close the door"]
       [else "Close the door" (define doorState "closed") ])]
    [(string=? key "l")
     (cond
       [(or (string=? doorState "opened") (string=? doorState "locked")) "Can't lock the door"]
       [else "Lock the door" (define doorState "locked") ])]
    [(string=? key "u")
     (cond
       [(or (string=? doorState "closed") (string=? doorState "opened")) "Can't unlock the door"]
       [else "Unlock the door" (define doorState "closed") ])]
       )
    )

;; green = opened, yellow = closed, red = locked
;; draw-door: String -> Image
(define (draw-door state)
  (cond
    [(string=? state "opened") (circle 20 "solid" "green")]
    [(string=? state "closed") (circle 20 "solid" "yellow")]
    [(string=? state "locked") (circle 20 "solid" "red")]
    )
  )

(check-expect (draw-door "opened") (circle 20 "solid" "green"))

;; BigBang




