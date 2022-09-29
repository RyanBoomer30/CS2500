#lang slideshow
(require 2htdp/image)

(define (starFlag starLength starColor recWidth recHeight recColor)
  (overlay(star starLength "solid" starColor)
          (rectangle recWidth recHeight "solid" recColor)
  )
 )

; Input (vietnam width height) to draw a flag of Vietnam
(define (vietnam w h)
  (starFlag (* (min w h) 0.5) "yellow" w h "red")
 )

; Input (chile width height) to draw a flag of Chile
(define (chile w h)
  (overlay/offset
                 (overlay/offset(starFlag (* (min (* w 0.3) (* h 0.5)) 0.5) "white" (* w 0.3) (* h 0.5) "blue")
                                (* w 0.5) 0
                                (rectangle (* w 0.7) (* h 0.5) "solid" "white")
                                )
                 0 (* h 0.5)
                 (rectangle w (* h 0.5) "solid" "red")
  )
)

; Input (suriname width height) to draw a flag of Suriname
(define (suriname w h)
  (overlay/align "middle" "bottom"
                 (rectangle w (* h 0.2) "solid" "darkgreen")
                 (rectangle w (* h 0.3) "solid" "white")
                 (overlay/align "middle" "top"
                                (vietnam w (* h 0.4))
                                (rectangle w (* h 0.7) "solid" "red")
                 )
                 
                 (rectangle w (* h 0.8) "solid" "white")
                 (rectangle w h "solid" "darkgreen")
   )
)

; Input (saintLucia width height) to draw a flag of Saint Lucia
(define (saintLucia w h)
  (overlay(overlay/align "middle" "bottom"
                         (triangle/aas 300 300 (* 2 (* h 0.8) (cos (degrees->radians 75))) "solid" "yellow")
                         (isosceles-triangle (* h 0.7) 30 "solid" "black")
                         (isosceles-triangle (* h 0.8) 30 "solid" "white")
          )
          (rectangle w h "solid" "cyan")
          
  )
)

; Input (turkey width height) to draw a flag of Turkey
(define (turkey w h)
  (overlay(overlay/offset
           (rotate 30 (star (* (min w h) 0.15) "solid" "white"))
           
           (* (min w h) -0.45) 0
           
           (overlay/align "right" "middle"
                          (circle (* (min w h) 0.25) "solid" "red")
                          (circle (* (min w h) 0.3) "solid" "white"))
           
           )
  (rectangle w h "solid" "red")
  )
)

(vietnam 300 200)
(chile 300 200)
(suriname 300 200)
(saintLucia 300 200)
(turkey 300 200)