;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A CallType is one of:
;; - "zoom"
;; - "teams"
;; - "phone"
;; Interpretation: a type of call

(define CT-ZOOM "zoom")
(define CT-TEAMS "teams")
(define CT-PHONE "phone")

(define (calltype-temp ct)
  (cond
    [(string=? ct CT-ZOOM) ...]
    [(string=? ct CT-TEAMS) ...]
    [(string=? ct CT-PHONE) ...]))

(define-struct call [type duration attendees description])
(define-struct mtg [duration attendees description])
(define-struct alone [duration description])

(define E-ZOOM-DOC
  (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :("))
(define E-TEAMS-OH
  (make-call CT-TEAMS 7 (list "Mike" "Tajel")
             "Office hours with my partner to ask clarifying questions about the Design Recipe!"))
(define E-PHONE-SPAM
  (make-call CT-PHONE 1 (list "Unknown")
             "Who calls!? I think it was a scam..."))
;; These are characters from a TV show called "Friends", which was popular in
;; the 90s, which is when many of your instructors grew up.
(define E-MTG-STUDY
  (make-mtg 62 (list "Rachel" "Ross" "Joey" "Phoebe" "Chandler" "Monica")
            "Getting ahead on studying for Exam 2!"))
(define E-MTG-ADVISOR
  (make-mtg 28 (list "Ali")
            "Meeting with advisor to talk about a combined major"))

(define E-ALONE-LUNCH
  (make-alone 34 "Lunch"))
(define E-ALONE-READING
  (make-alone 25 "The Three-Body Problem isn't going to read itself!"))
(define LOE-1
  (list E-ZOOM-DOC E-ALONE-READING E-PHONE-SPAM
        E-ALONE-LUNCH E-TEAMS-OH E-MTG-ADVISOR E-MTG-STUDY))

;; An Event is one of:
;; - (make-call CallType PosInt [NEList-of String] String)
;; - (make-mtg PosInt [NEList-of String] String)
;; - (make-alone PosInt String)

;; Interpretation: an event in some period of time, which is either:
;; - A call using some technology, lasting some number of minutes with attendees
;;  (by name), and a description;
;; - An in-person meeting lasting some number of minutes
;;   with attendees (by name) and a description; or
;; - Time spent alone for some number of minutes with a description.

;; social-time : [List-of Event] -> PosInt
;; how much time was spent on calls and meetings?
(define (social-time lst)
  (+ (foldr + 0 (map call-duration
                     (filter (lambda (x) (call? x))lst)))
     (foldr + 0 (map mtg-duration
                     (filter (lambda (x) (mtg? x))lst)))))

(check-expect (social-time (list E-ZOOM-DOC E-TEAMS-OH E-MTG-STUDY E-MTG-ADVISOR E-ALONE-LUNCH LOE-1))
              119)
(check-expect (social-time (list E-ALONE-LUNCH LOE-1))
              0)
(check-expect (social-time '())
              0)

;; anything-but-phone? : [List-of Event] -> Boolean
;; makes sure there were no phone calls
(define (anything-but-phone? lst)
  (andmap (lambda (x) (not(call? x))) lst)
  )

(check-expect (anything-but-phone? (list E-ZOOM-DOC E-TEAMS-OH)) #false)
(check-expect (anything-but-phone? (list E-ALONE-LUNCH LOE-1)) #true)
(check-expect (anything-but-phone? (list E-ZOOM-DOC E-TEAMS-OH E-MTG-STUDY E-MTG-ADVISOR E-ALONE-LUNCH LOE-1)) #false)

;; peeps : [List-of Event] -> [List-of String]
;; alphabetized list from all event attendees
(define (peeps lst)
  (sort
   (append
    (foldr append '() (map call-attendees (filter (lambda (x) (call? x))lst)))
    (foldr append '() (map mtg-attendees (filter (lambda (x) (mtg? x))lst))))
   string<=?))

(check-expect (peeps (list E-ZOOM-DOC E-TEAMS-OH E-MTG-STUDY E-MTG-ADVISOR E-ALONE-LUNCH LOE-1))
              (list "Ali" "Chandler" "Dr. Zoidberg" "Joey" "Mike" "Monica" "Phoebe" "Rachel" "Ross" "Tajel"))
(check-expect (peeps (list E-ALONE-LUNCH LOE-1))
              '())
(check-expect (peeps '())
              '())