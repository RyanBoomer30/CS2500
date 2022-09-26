;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |0|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

#| String Processing |#

;; Template
(define (concat-longer-first-tempalte string1 string2)
  (cond
    [ (< (string-length string1) (string-length string2)) ...]
    [else ...]
    )
  )
;; concat-longer-first: String, String -> String
;; Given two strings, concatenate them into one string as long as the longer is in the front
(define (concat-longer-first string1 string2)
  (cond
    [ (< (string-length string1) (string-length string2)) (string-append string2 " " string1)]
    [else (string-append string1 " " string2)]
    )
  )

(check-expect (concat-longer-first "hello" "there") "hello there")
(check-expect (concat-longer-first "hi" "there") "there hi")
(check-expect (concat-longer-first "hello" "mom") "hello mom")

#| The Canadian |#

;; A CanadianTrainStop is one of:
;; "Vancouver"
;; "Kamloops"
;; "Jasper"
;; "Edmonton"
;; "Saskatoon"
;; "Winnipeg"
;; "Sioux Lookout"
;; "Sudbury Jct."
;; "Toronto"
;; represents one of the stops in VIA Rail Canadian

(define CanadianTrainStop "Vancouver")
(define CanadianTrainStop2 "Kamloops")
(define CanadianTrainStop3 "Jasper")

;; Template
(define (CanadianTemplate StopName)
  (cond
    [(string=? StopName "Vancouver")...]
    [(string=? StopName "Kamloops")...]
    [(string=? StopName "Jasper")...]
    [(string=? StopName "Edmonton")...]
    [(string=? StopName "Saskatoon")...]
    [(string=? StopName "Winnipeg")...]
    [(string=? StopName "Sioux Lookout")...]
    [(string=? StopName "Sudbury Jct.")...]
    [(string=? StopName "Toronto")...]
    [else ...]
    )
  )

;; province is a function that take in the name of a stop and output its province in Canada
;; province: String -> ProvinceName (string)
(define (province StopName)
  (cond
    [(or (string=? StopName "Vancouver") (string=? StopName "Kamloops")) "British Columbia"]
    [(or (string=? StopName "Jasper") (string=? StopName "Edmonton")) "Alberta"]
    [(string=? StopName "Saskatoon") "Saskatchewan"]
    [(string=? StopName "Winnipeg") "Manitoba"]
    [(or (string=? StopName "Sioux Lookout") (string=? StopName "Sudbury Jct.")
         (string=? StopName "Toronto")) "Ontario"]
    [else "Not a valid stop"]
    )
  )

(check-expect (province CanadianTrainStop) "British Columbia")

;; can-transfer? is a function that take in the name of a stop and output whether or not it has a transfer line
;; can-transfer?: String -> Boolean
(define (can-transfer? StopName)
  (cond
    [(or (string=? StopName "Jasper") (string=? StopName "Winnipeg") (string=? StopName "Toronto")) #true]
    [else #false])
  )

(check-expect (can-transfer? CanadianTrainStop) #false)

#| Compass |#

;; A CompassDirection is one of the:
;; "North"
;; "South"
;; "East"
;; "West"
;; "NorthEast"
;; "NorthWest"
;; "SouthEast"
;; "SouthWest"
;; CompassDirection is a data definition that represents a single cardinal or inter-cardinal direction

(define CompassDirection "North")
(define CompassDirection2 "South")
(define CompassDirection3 "NorthEast")


;; Template
(define (cardinal?-template Direction)
  (cond
    [(string=? Direction "North") ...]
    [(string=? Direction "South") ...]
    [(string=? Direction "East") ...]
    [(string=? Direction "West") ...]
    [(string=? Direction "NorthEast") ...]
    [(string=? Direction "NorthWest") ...]
    [(string=? Direction "SouthEast") ...]
    [(string=? Direction "SouthWest") ...]
    [else ...]
    )
  )

;; cardinal? is a function that determines if a CompassDirection is a cardinal direction
;; cardinal?: String -> Boolean
(define (cardinal? Direction)
  (cond
    [(or (string=? Direction "North") (string=? Direction "South")
         (string=? Direction "East") (string=? Direction "West")) #true]
    [else #false]
    )
  )

(check-expect (cardinal? CompassDirection) #true)

#| Rotten Tomatoes |#

(define-struct movie [name tomatometer rating])
;; A (Movie name tomatometer rating) represents a movie with name "name", tomatometer percentage of "tomatometer" and
;; MPAA rating as "rating"

(define Titanic (make-movie "Titanic" 87 "PG-13"))
(define StarWars (make-movie "Star Wars" 93 "PG"))
(define Avenger (make-movie "Avenger" 93 "G"))

;; Template
(define (movie-template s)
  (... (movie-name s) ...
       (movie-tomatometer s) ...
       (movie-rating s) ...
  ))

;; can-and-will-watch? is a function that determines whether or not a Movie is age-appropriate
;; can-and-will-watch?: List (String int int) -> Boolean
(define (can-and-will-watch? movie)
  (cond
    [(or (string=? (movie-rating movie) "PG-13")(string=? (movie-rating movie) "G")) #true]
    [else #false]
    )
  )

(check-expect (can-and-will-watch? Titanic) #true)

;; Draw a rotten tomato info panel
;; movie->image: List (String int int) -> Image
(define (movie->image movie)
  (overlay (overlay/offset (text (movie-name movie) 30 "red")
                          0 70
                         (overlay/offset (text (movie-rating movie) 13 "black")
                                          0 -20
                                          (text (string-append (number->string (movie-tomatometer movie)) "% Rating")
                                                15 "black")))
           (rectangle 100 200 "solid" "white")
           ))

(check-expect (movie->image Titanic)
              (overlay (overlay/offset (text "Titanic" 30 "red")
                                       0 70
                                       (overlay/offset (text "PG-13" 13 "black")
                                                       0 -20
                                                       (text "87% Rating" 15 "black")))
                       (rectangle 100 200 "solid" "white")
                       )
              )

