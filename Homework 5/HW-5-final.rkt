;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW-5-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require "io-extra.rkt")

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
    [(cons? lst) (cons (first lst) (take-n (- n 1) (rest lst)))]))

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
    [(cons? lst) (if (> n 0) (drop-n (- n 1) (rest lst)) lst)]))

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
         null ;;<--- ask about
         )]))
(check-expect (take-while positive? (list 1 2 3 4 5 -1 6 7 8 9 10)) (list 1 2 3 4 5))
(check-expect (take-while string? (list "a" "b" "c" "d" 1 "e" "f")) (list "a" "b" "c" "d"))
(check-expect (take-while positive? '()) '())

;; drop-while : (X -> Bool) [List-of X] -> [List-of X]
;; (drop-while pred alist) produces a suffix of alist that we get by dropping
;; the longest prefix of alist that satisfies pred.

(define (drop-while pred alist)
  (cond
    [(empty? alist) '()]
    [(cons? alist)
     (if (pred (first alist)) (drop-while pred (rest alist)) (rest alist) ;;<--- ask about
         )]))

(check-expect (drop-while positive? (list 1 2 3 4 5 -1 6 7 8 9 10)) (list 6 7 8 9 10))
(check-expect (drop-while string? (list "a" "b" "c" "d" 1 "e" "f")) (list "e" "f"))
(check-expect (drop-while positive? '()) '())

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
            (cons (same-group-list alist)
                  (group-by same-group? (drop-n (length (same-group-list alist)) alist)))])))

(check-expect (group-by = (list 10 10 20 20 20 10)) (list (list 10 10) (list 20 20 20) (list 10)))
(check-expect (group-by < (list 20 30 40 30 50 20 30))
              (list (list 20 30 40) (list 30 50) (list 20 30)))

;; part 2

;; table-of-contents : [List-of String] -> [List-of String]
;; Extracts the table of contents from the lines of text of a book, formatted
;; similarly to pg100.txt.
(define (table-of-contents los)
  (local [;; not-contain-content : [List-of String] -> Boolean
          ;; check if a list doesn't contain the string "Contents"
          (define (not-contain-content str)
            (not (string-contains? "Contents" str)))
          ;; not-empty-string : [List-of String] -> Boolean
          ;; check if a string isn't empty
          (define (not-empty-string str)
            (not (string=? str "")))
          ;; substring-title : String -> String
          ;; remove the empty spaces in front of the string
          (define (substring-title str)
            (substring str 4))]
         (cond
           [(empty? los) '()]
           [(cons? los)
            (map substring-title
                 (take-while not-empty-string (rest (drop-while not-contain-content los))))])))

(check-expect (table-of-contents (read-lines "pg100-mod.txt"))
              (list "THE SONNETS" "ALL’S WELL THAT ENDS WELL" "THE TRAGEDY OF ANTONY AND CLEOPATRA"))
(check-expect (table-of-contents '()) '())
(define-struct work [title contents])
;; A Work is a (make-work String [List-of String])
;; A (make-work title lines) represents a work of literature with the given
;; title and lines of text.

;; work-template : Work -> ?
(define (work-template w)
  (... (work-title w) ... (work-contents w) ...))

(define EX-WORK-1 (make-work "Hamlet" (list "To be or not to be.")))
(define EX-WORK-2 (make-work "Romeo and Juliet" (list "What's in a name?")))

;; extract-works : [List-of String] [List-of String] -> [List-of Work]
;; (extract-works toc lines) produces the works in a collection, given the list
;; of works extracted from the table of contents. The lines are formatted
;; similar to pg100.txt.

(define (extract-works toc lines)
  (local
   [;; not-after-toc : string -> boolean
    ;;checks whether or not a string is the last value of the table of contents
    (define (not-after-toc str)
      (not (string-contains? (first reverse-toc) str)))
    ;; reverse-toc : [List-of string] -> [List-of Strings]
    ;; returns the inverse of the table of contents
    (define reverse-toc (reverse (table-of-contents lines)))
    ;;before-end : string -> boolean
    ;; checks if the given string is not FINIS
    (define (before-end str)
      (not (string-contains? "FINIS" str)))
    ;; list-of-text : [List-of string] -> [List-of String]
    ;; returns everything in the file that comes after the table of contents and before the word "FINIS"
    (define list-of-text (take-while before-end (drop-while not-after-toc lines)))
    ;; is-title? : string -> boolean
    ;; checks if the given string is in the table of contents
    (define (is-title? str)
      (member? str toc))
    ;; is-not-title? : string -> boolean
    ;; checks if the given string is NOT in the table of contents
    (define (is-not-title? str)
      (not (member? str toc)))
    ;; create-work : [list-of string] -> Work
    ;;creates the struct for a list of strings
    (define (create-work lst)
      (make-work (first lst) (rest lst)))
    ;; end-of-work : string string -> booelan
    ;; check if the first string is not a title and second string is a title
    (define (end-of-work s1 s2)
      (not (and (is-not-title? s1) (is-title? s2))))
    ;; not-empty : string -> boolean
    ;; checks if the given string is not an empty string ("")
    (define (not-empty str)
      (not (string=? str "")))]
   (cond
     [(empty? toc) '()]
     [(cons? toc) (map create-work (group-by end-of-work (filter not-empty list-of-text)))])))

(check-expect (extract-works (table-of-contents (list ""
                                                      ""
                                                      "                    Contents"
                                                      ""
                                                      "    A"
                                                      "    B"
                                                      "    C"
                                                      ""
                                                      ""
                                                      "A"
                                                      "aaa"
                                                      ""
                                                      ""
                                                      "B"
                                                      "bb"
                                                      ""
                                                      ""
                                                      "C"
                                                      "cc"
                                                      ""
                                                      ""
                                                      "FINIS"
                                                      ""))
                             (list ""
                                   ""
                                   "                    Contents"
                                   ""
                                   "    A"
                                   "    B"
                                   "    C"
                                   ""
                                   ""
                                   "A"
                                   "aaa"
                                   ""
                                   ""
                                   "B"
                                   "bb"
                                   ""
                                   ""
                                   "C"
                                   "cc"
                                   ""
                                   ""
                                   "FINIS"
                                   ""))
              (list (make-work "    A" (list "    B" "    C"))
                    (make-work "A" (list "aaa"))
                    (make-work "B" (list "bb"))
                    (make-work "C" (list "cc"))))

;; (extract-works (table-of-contents (read-lines "pg100-mod.txt")) (read-lines "pg100-mod.txt"))

;; extract-works-to-files : String -> [List-of String]
;; Given the path to a collected works, writes several files -- one for each
;; work -- and produces the list of file names.
(define (extract-works-to-files works)
  (local [;; create-file : String [List-of String] -> PathString
          ;; create a file given its name and content
          (define (create-file name content)
            (write-lines (string-append name ".txt") content))]
         (map create-file (map work-title works) (map work-contents works))))

;; top-n-words : Int [List-of String] -> [List-of String]
;; (top-n-words n word-list) produces the n most frequent words in the given
;; word-list
(define (top-n-words n word-list)
  (local [;; ordered-list : [List-of String] -> [List-of String]
          ;; sort a list based on their lexicographically
          (define ordered-list (sort word-list string<=?))
          ;; length-compared : [List-of String] [List-of String] -> Boolean
          ;; check if the length of the first list is larger than the length of the second list
          (define (length-compared lst1 lst2)
            (> (length lst1) (length lst2)))]
         (cond
           [(empty? word-list) '()]
           [else
            (cond
              [(empty? word-list) '()]
              [(cons? word-list)
               (take-n n (map first (sort (group-by string=? ordered-list) length-compared)))])])))

(check-expect (top-n-words 4 (list "a" "a" "b" "c" "b" "c" "c")) (list "c" "a" "b"))
(check-expect (top-n-words 1 (list "a" "a" "b" "c" "b" "c" "c")) (list "c"))
(check-expect (top-n-words 0 (list "a" "a" "b" "c" "b" "c" "c")) '())

;; word-frequency : [List-of String] [List-of String] -> [List-of Int]
;; (word-frequency top-words word-list) produces a list that counts
;; the number of occurrences of each word in top-words in word-list.
;; The list.
(define (word-frequency top-words word-list)
  (local
   [;; top-words-filter : String -> Boolean
    ;; check if a word belongs to a list
    (define (top-words-filter word)
      (member? word top-words))
    ;; filter-topword: [List-of [List-of String]] [List-of String] -> [List-of [List-of String]]
    ;; filter in all the top-words that are not in the [List-of [List-of String]]
    (define (filter-topword lst top-word-lst)
      (cond
        [(empty? lst) '()]
        [(empty? top-word-lst) '()]
        [(cons? lst)
         (cons (if (member? (first top-word-lst) (map (lambda (x) (if (empty? x) x (first x))) lst))
                   (first (filter (lambda (x) (string=? (first top-word-lst) (first x))) lst))
                   '())
               (filter-topword lst (rest top-word-lst)))]))]
   (cond
     [(empty? word-list) '()]
     [(cons? word-list)
      (map length
           (filter-topword (group-by string=? (filter top-words-filter (sort word-list string<=?)))
                           top-words))])))

(check-expect (word-frequency (top-n-words 3 (list "a" "a" "b" "c" "b" "c" "c"))
                              (list "a" "a" "b" "c" "b" "c" "c"))
              (list 3 2 2))
(check-expect (word-frequency (top-n-words 1 (list "a" "a" "b" "c" "b" "c" "c"))
                              (list "a" "a" "b" "c" "b" "c" "c"))
              (list 3))
(check-expect (word-frequency (top-n-words 0 (list "a" "a" "b" "c" "b" "c" "c"))
                              (list "a" "a" "b" "c" "b" "c" "c"))
              '())

;; document-distance : [List-of String] [List-of String] [List-of String] -> Number
;; (document-distance top-words word-list-1 word-list-2) calculates the
;; distance between the word frequency lists of the given files.
(define (document-distance top-words word-list-1 word-list-2)
  (local
   [;; distance : (frequancy of word in word-list-1) (frequancy of word in word-list-2) -> number
    ;; Calculates the distance between two words
    (define (distance x y)
      (expt (- x y) 2))]
   (sqrt (foldr +
                0
                (map distance
                     (word-frequency top-words word-list-1)
                     (word-frequency top-words word-list-2))))))

(check-expect (document-distance (top-n-words 2 (list "a" "a" "b" "c" "b" "c" "c"))
                                 (list "a" "a" "b" "c" "b" "c" "c")
                                 (list "a" "a" "b" "c" "b" "c" "c"))
              0)
(check-expect (document-distance (top-n-words 2 (list "a" "a" "b" "c" "b" "c" "c"))
                                 (list "a" "a" "b" "c" "b" "c" "c")
                                 (list "a" "b" "b" "b" "b" "a" "c"))
              2)
(check-expect
 (document-distance (top-n-words 2 (list "a" "a" "b" "c" "b" "c" "c")) (list "") (list ""))
 0)

;; top-n-words/file : Int PathString -> [List-of String]
;; Applies top-n-words to the contents of a file.
(define (top-n-words/file n path)
  (top-n-words n (read-words path)))

;; document-distance/file : [List-of String] PathString PathString -> Number
;; Uses document-distance and word-frequency to calculate the distance between
;; two files.
(define (document-distance/file top-words path-1 path-2)
  (document-distance top-words (read-words path-1) (read-words path-2)))
(define-struct similar-works [work distance])

;; A similar-works is a (make-similar-works Work Number)
;; A (make-similar-works Work Number) represents a work and their distance to another word

;; similar-works-template : similar-works -> ?
(define (similar-works-template w)
  (... (similar-works-work w) ... (similar-works-distance w) ...))

;; sort-by-distance : number pathString -> [List-of Works]
;;sorts a list of similar works in order of similarity
(define (sort-by-distance n Article)
  (local
   [;; test-distance : number pathString pathString -> [List-of works]
    ;; returns a list of works that are similar to the given article
    (define (test-distance n firstArticle allArticles)
      (cond
        [(empty? allArticles) '()]
        [(cons? allArticles)
         (cons (make-similar-works
                (make-work (string-append (work-title (first allArticles)) ".txt")
                           (read-lines (string-append (work-title (first allArticles)) ".txt")))
                (document-distance/file (top-n-words/file n (string-append firstArticle ".txt"))
                                        (string-append firstArticle ".txt")
                                        (string-append (work-title (first allArticles)) ".txt")))
               (test-distance n firstArticle (rest allArticles)))]))]
   (map similar-works-work
        (sort (test-distance n
                             Article
                             (extract-works (table-of-contents (read-lines "pg100.txt"))
                                            (read-lines "pg100.txt")))
              (lambda (article1 article2)
                (< (similar-works-distance article1) (similar-works-distance article2)))))))

(sort-by-distance 10 "KING JOHN")
(sort-by-distance 10 "THE TRAGEDY OF MACBETH")
(sort-by-distance 10 "THE COMEDY OF ERRORS")
