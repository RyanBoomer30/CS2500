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
(define tree4 (make-leaf "one"))
(define tree5 (make-node (make-leaf "two") (make-leaf "three")))
(define tree6 (make-node (make-leaf "one") tree5))

;; tree-add-all : Number [BinTree Number] -> [BinTree Numbers]
;; (tree-add-all n tree) adds n to every value in tree
(define (tree-add-all n tree)
  (tree-modify n tree +))

(check-expect (tree-add-all 5 tree1) (make-leaf 10))
(check-expect (tree-add-all 5 tree2) (make-node (make-leaf 10) (make-leaf 11)))
(check-expect (tree-add-all 5 tree3) (make-node (make-leaf 10) (make-node (make-leaf 10) (make-leaf 11))))

;; tree-append-all : String [BinTree String] -> [BinTree String]
;; (tree-append-all s tree) adds s as a *suffix* to every string in the tree
(define (tree-append-all s tree)
  (tree-modify s tree string-append))

(check-expect (tree-append-all " two" tree4) (make-leaf "one two"))
(check-expect (tree-append-all " four" tree5) (make-node (make-leaf "two four") (make-leaf "three four")))
(check-expect (tree-append-all " five" tree6) (make-node (make-leaf "one five") (make-node (make-leaf "two five")
                                                                                           (make-leaf "three five"))))
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
(define-struct employee [name title direct-report])
(define-struct group [group-name members])

;; An OrgChart is one of:
;; - (make-employee string string (List-of group))
;; - (make-group string (List-of employee))

(define Uta (make-employee "Uta Poiger" "Social Sciences & Humanities" '()))
(define Carmen (make-employee "Carmen Sceppa" "Bouve" '()))
(define Alan (make-employee "Alan Mislove" "Khoury" '()))
(define Thomas (make-employee "Thomas Sheahan" "Curriculum & Programs" '()))
(define Karl (make-employee "Karl Reid" "Chief Inclusion Officer" '()))
(define Madeleine (make-employee "Madeleine Estabrook" "Student Affairs" '()))
(define David (make-employee "David Madigan" "Academic Affairs"
                             (list (make-group "Administration"
                                               (list Thomas))
                                   (make-group "Academic Deans"
                                               (list Alan Carmen Uta)))))
(define Joseph (make-employee "Joseph E. Aoun" "President"
                              (list (make-group "The Cabinet"
                                                (list David Madeleine Karl)))))
(define Cabinet (make-group "The Cabinet" (list David Madeleine Karl)))
(define Administration (make-group "Administration" (list Thomas)))
(define Academic (make-group "Academic Deans" (list Alan Carmen Uta)))

;; org-chart-template : OrgChart -> ?
(define (org-chart-template org)
  (cond
    [(employee? org) (... (employee-name org) ...
                          (employee-title org) ...
                          (if (empty? (employee-direct-report org)) ... ...))]       
    [(group? org)
     (...
      (group-group-name org) ...
      (if (empty? (group-members org)) ... ... ))]))

;; sub-group : Function OrgChart X Y -> (List-of OrgChart)
;; recursive function that run a function through all the sub-elements of a tree
(define (sub-group f lst p1 p2)
  (cond
    [(empty? lst) '()]
    [(cons? lst)
     (if (string=? p1 "")
         (cons (f (first lst))
               (sub-group f (rest lst) p1 p2))
         (if (string=? p2 "")
             (cons (f (first lst) p1)
                   (sub-group f (rest lst) p1 p2))
             (cons (f (first lst) p1 p2)
                   (sub-group f (rest lst) p1 p2))))]))

;; full-title : OrgChart String -> OrgChart
;; adds the “(organization name)” to the title for each person in the OrgChart
(define (full-title org orgname)
  (abstraction-1 org full-title orgname ""))

(check-expect (full-title '() " Northeastern") '())
(check-expect (full-title Alan " Northeastern") (make-employee "Alan Mislove" "Khoury Northeastern" '()))
(check-expect (full-title David " Northeastern")
              (make-employee
               "David Madigan"
               "Academic Affairs Northeastern"
               (list
                (make-group "Administration"
                            (list (make-employee "Thomas Sheahan" "Curriculum & Programs Northeastern" '())))
                (make-group "Academic Deans"
                            (list (make-employee "Alan Mislove" "Khoury Northeastern" '())
                                  (make-employee "Carmen Sceppa" "Bouve Northeastern" '())
                                  (make-employee "Uta Poiger" "Social Sciences & Humanities Northeastern" '()))))))

;; name-change : OrgChart String String -> OrgChart
;; (name-change org old-dept-name new-dept-name) renames old-dept-name to
;; new-dept-name, for every group in the organization.
(define (name-change org old-dept-name new-dept-name)
  (abstraction-1 org name-change old-dept-name new-dept-name))

(check-expect (name-change '() "The Cabinet" "Admin") '())
(check-expect (name-change David "The Cabinet" "Admin")
              (make-employee
               "David Madigan"
               "Academic Affairs"
               (list
                (make-group "Administration"
                            (list (make-employee "Thomas Sheahan" "Curriculum & Programs" '())))
                (make-group "Academic Deans"
                            (list (make-employee "Alan Mislove" "Khoury" '())
                                  (make-employee "Carmen Sceppa" "Bouve" '())
                                  (make-employee "Uta Poiger" "Social Sciences & Humanities" '()))))))
(check-expect (name-change David "Administration" "Admin")
              (make-employee
               "David Madigan"
               "Academic Affairs"
               (list
                (make-group "Admin"
                            (list (make-employee "Thomas Sheahan" "Curriculum & Programs" '())))
                (make-group "Academic Deans"
                            (list (make-employee "Alan Mislove" "Khoury" '())
                                  (make-employee "Carmen Sceppa" "Bouve" '())
                                  (make-employee "Uta Poiger" "Social Sciences & Humanities" '()))))))

;; abstraction-1 : OrgChart Function String String -> OrgChart 
(define (abstraction-1 org f v1 v2)
  (cond
    [(empty? org) org]
    [(employee? org) (make-employee
                      (employee-name org)
                      (if (string=? v2 "")
                          (string-append (employee-title org) v1)
                          (employee-title org)
                          )
                      (if (empty? (employee-direct-report org))
                          '()
                          (sub-group f (employee-direct-report org) v1 v2)))]
    [(group? org)
     (make-group
      (if (string=? (group-group-name org) v1)
          v2
          (group-group-name org))
      (if (empty? (group-members org))
          '()
          (sub-group f (group-members org) v1 v2)))])

  )

;; department-equal? : Group String -> Number
;; return the length of the list of members in a group
(define (department-equal? org dept)
  (cond
    [(employee? org) 0]
    [(group? org)
     (if (string=? (group-group-name org) dept)
         (length (group-members org))
         0
         )]
    ))

;; num-peeps : OrgChart -> Number
;; counts how many people are in an OrgChart
(define (num-peeps org)
  (abstraction-2 org "" + 0 1)
  )
 
(check-expect (num-peeps Joseph) 8)
(check-expect (num-peeps David) 5)
(check-expect (num-peeps Alan) 1)

;; count-department : OrgChart String -> Number
;; (count-department org dept) produces the number of people in org from
;; the dept department (i.e., group name).
(define (count-department org dept)
  (abstraction-2 org dept + department-equal? department-equal?)
  )


(check-expect (count-department Joseph "The Cabinet") 3)
(check-expect (count-department Joseph "Administration") 1)
(check-expect (count-department Joseph "Academic Deans") 3)

;; all-departments : OrgChart -> [List-of String]
;; (all-departments org) produces the list of departments (i.e., group names) in the 
;; organization.
(define (all-departments org)
  (abstraction-2 org "" append '() '())
  )

(check-expect (all-departments Joseph) (list "Administration" "Academic Deans" "The Cabinet"))
(check-expect (all-departments David) (list "Administration" "Academic Deans"))
(check-expect (all-departments Alan) '())

;; abstraction-2 : OrgChart String Operation Variable Variable
(define (abstraction-2 org dept op v1 v2)
  (local [;; org-recurssion : OrgChart -> Function
          ;; Run the input function on an OrgChart
          (define (org-recurssion department acc) 
            (op (abstraction-2 department dept op v1 v2) acc)
            )]
    (cond
      [(group? org) (foldr org-recurssion (if (empty? v1) (list (group-group-name org))
                                              (if (string=? dept "")
                                                  v1
                                                  (v1 org dept)
                                                  )
                                              )(group-members org))]
      [(employee? org) (foldr org-recurssion (if (string=? dept "")
                                                 v2
                                                 (v1 org dept)
                                                 )(employee-direct-report org))])))