;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct leaf [value])
(define-struct node [left right])
;; A [BinTree X] is one of:
;; - (make-leaf X)
;; - (make-node [BinTree X] [BinTree X])

;; bintree-template : BinTree -> ?
(define (bintree-template t)
  (cond
    [(leaf? t) (... (leaf-value t) ...)]
    [(node? t)
     (... (bintree-template (node-left t)) ...
          (bintree-template (node-right t)) ...)]))

;; three examples of [BinTree X]
(define tree1 (make-leaf 5))
(define tree2 (make-node (make-leaf 5) (make-leaf 6)))
(define tree3 (make-node (make-leaf 5) tree2))

;; tree-add-all : Number [BinTree Number] -> [BinTree Numbers]
;; (tree-add-all n tree) adds n to every value in tree
(define (tree-add-all n tree)
  (tree-modify n tree +))

;; tree-append-all : String [BinTree String] -> [BinTree String]
;; (tree-append-all s tree) adds s as a *suffix* to every string in the tree
(define (tree-append-all s tree)
  (tree-modify s tree string-append))

;; (tree-modify x tree f) : Variable [BinTree X] Function -> [BinTree X]
;; A helper function that abstracts the differences between (tree-add-all and tree-append-all)
;; (tree-modify x tree f) calls function f with the parameter x and every leaf in the tree
(define (tree-modify x tree f)
  (cond
    [(leaf? tree) (make-leaf (f (leaf-value tree) x))]
    [(node? tree)
     (make-node (tree-modify x (node-left tree) f)
                (tree-modify x (node-right tree) f))])
  )

#|--------------------------------------------------------------------------------------------------------------------|#
(define-struct employee [name title])
(define-struct group [groupName members])
;; An OrgChart is one of:
;; - (make-employee name title)
;; - (make-group groupName members)

(define (org-chart-template t)
  (cond
    [(employee? t) (... (employee-name) ...
                        (employee-title) ...)]
    [(group? t)
     (... (group-groupName) ...
          (org-chart-template (list-of-tree-template (group-members t))) ...)
     ]
    )
)

;; list-of-tree-template : [List-of Tree] -> ?
(define (list-of-tree-template lst)
  (cond
    [(empty? l) ...]
    [(cons? l)
     (... (first lst) ... (list-of-tree-template (rest lst)))
     ]
    )
  )