;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; PART 1 =====

;; An ELGraph is a [List-of Node]

(define-struct node [label edges])
;; A Node is a (make-node String [List-of Edges])
;; A (make-node label edges) represents a node with a specific label and
;; a list of edges of which it is the head node.

(define-struct edge [label tail])
;; An Edge is a (make-edge String String)
;; A (make-edge label tail) represents an edge with a specific label and
;; the label of its tail node.

(define EX-GRAPH-0 (list (make-node "A" empty)))

(define EX-GRAPH-1 (list (make-node "A" (list (make-edge "A-B" "B") (make-edge "A-C" "C")))
                         (make-node "B" (list (make-edge "B-A" "A")))
                         (make-node "C" empty)))

(define EX-GRAPH-2 (list (make-node "A" (list (make-edge "A-B" "B")))
                         (make-node "B" (list (make-edge "B-C" "C")))
                         (make-node "C" (list (make-edge "C-A" "A")))))

;; PART 2 =====

(define given-street-graph
  (list
   (make-node "Gainsborough and St. Stephen"
              (make-edge "west" "Opera and St. Stephen")
              (make-edge "north" "Hemenway and Gainsborough"))
   (make-node "Opera and St. Stephen"
              (make-edge "south" "Huntington and Opera"))
   (make-node "Hemenway and Gainsborough"
              (make-edge "west" "Hemenway and Forsyth St"))
   (make-node "Huntington and Opera"
              (make-edge "west" "Huntington and Forsyth St"))
   (make-node "Hemenway and Forsyth St"
              (make-edge "west" "Hemenway and Forsyth Way")
              (make-edge "east" "Hemenway and Gainsborough")
              (make-edge "south" "Huntington and Forsyth St"))
   (make-node "Huntington and Forsyth St"
              (make-edge "north" "Hemenway and Forsyth St")
              (make-edge "east" "Huntington and Opera")
              (make-edge "west" "Huntington and Forsyth Way"))
   (make-node "Hemenway and Forsyth Way"
              (make-edge "south" "Huntington and Forsyth Way")
              (make-edge "east" "Hemenway and Forsyth St"))
   (make-node "Huntington and Forsyth Way"
              (make-edge "north" "Hemenway and Forsyth Way")
              (make-edge "east" "Huntington and Forsyth St"))))

(define my-street-graph
  (list
   (make-node "Tremont and Cyprians"
              (make-edge "north" "Columbus and Cyprians")
              (make-edge "east" "Tremont and Cunard"))
   (make-node "Columbus and Cyprians"
              (make-edge "east" "Columbus and Cunard"))
   (make-node "Tremont and Cunard"
              (make-edge "west" "Tremont and Cyprians")
              (make-edge "east" "Tremont and Coventry"))
   (make-node "Columbus and Cunard"
              (make-edge "south" "Tremont and Cunard")
              (make-edge "west" "Columbus and Cyprians")
              (make-edge "east" "Columbus and Coventry"))
   (make-node "Tremont and Conventry"
              (make-edge "west" "Tremont and Cunard")
              (make-edge "north" "Columbus and Coventry"))
   (make-node "Columbus and Coventry"
              (make-edge "west" "Columbus and Cunard"))))

;; Part 3 =====
;; driving-directions: ELGraph String String -> [List-of String]
;; driving-directions consumes
;; (1) a graph of streets and directions (e.g., from the previous exercise),
;; (2) the starting point, which is an intersection,
;; (3) the destination, which is another intersection
(define (driving-directions Graph start end)
  (local [(define (dfs inprogress visited neighbor)
            (cond
              [(empty? neighor) null]
              [(string=? (first inprogress) end) inprogress]
              [else
               ...
               ]
              )
            )]
    (in-progress '() Graph) 
)

(check-expect
    (driving-directions given-street-graph
                        "Forsyth Way and Huntington" 
                        "St Stephen and Gainsborough")
    '("east to Forsyth St and Huntington"
    "east to Opera and Huntington"
    "north to Opera and St Stephen"
    "east to St Stephen and Gainsborough"))))