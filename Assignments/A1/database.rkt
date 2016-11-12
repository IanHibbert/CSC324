#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***
Ian Hibbert, hibberti, 1000632871
<Name>, <CDF>, <ID>
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))


; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         SELECT
         replace
         name-tables
         where
         order)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  (car table))

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  (rest table))

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  (length (tuples table)))



; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes:
   - a list
   - an item

and returns #t if the item is in the list
and #f otherwise
|#
(define (find-item lst item)
  (if (empty? lst)
      #f
      (if (equal? item (first lst))
          #t
          (find-item (rest lst) item))))


#|
A function that takes:
   - an element
   - a list
   - returns index of element or #f if the element doesn't exist
|#
(define (list-index lst e)
  (cond [(empty? lst) #f]
        [(equal? e (first lst)) 0]
        [(equal? (find-item (rest lst) e) #f) #f]
        [else (add1 (list-index (rest lst) e))]))



#|
(cartesian-product table1 table2)
  table1: list of lists [K1, K2, ..., Km]
  table2: list of lists [L1, L2, ..., Ln]

  Returns a list of the contatenation of all possible pairs of lists, in the 
  order [K1 + L1, K1 + L2, ..., K1 + Ln, K2 + L1, ..., K2 + Ln, ..., Km + Ln]

  If at least one of 'table1' and 'table2' is empty, their Caretesian product
  does not contain any lists.

> (cartesian-product '((1 4) (2 10)) '((3 4 5) (2)))
'((1 4 3 4 5) (1 4 2) (2 10 3 4 5) (2 10 2))
|#
(define (cartesian-product table1 table2)
  (if (or (empty? table1) (empty? table2))
      '()
      (append (map (λ (x)
                     (append (first table1) x)) table2)
              (cartesian-product (rest table1) table2))
      ))

;Helper functions for SELECT
#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (find-attr attr-list attr tuple)
  (list-ref tuple (list-index attr-list attr)))


#|
A function that takes: 
  - a list of attributes
  - a list of required attributes
  - a tuple

  and returns the value of the tuples corresponding to that attribute.

> (filter-tuple (attributes Person) '("Name" "Age") (first (tuples Person)))

|#
(define (filter-tuple attr-list req-list tuple)
  (map (lambda (attr)
         (find-attr attr-list attr tuple))
       req-list))


#|
A function that takes:
   - a list of attributes
   - a table

and returns a table where tuples that correspond to the attributes match
|#
(define (select-table attrs table)
  (cons attrs
        (map (lambda (tuple)
               (filter-tuple (attributes table) attrs tuple))
             (tuples table))))

;SELECT with Multiple Tables
#|
A function that takes:
   - a list of lists of attributes 

and returns a list of duplicate attributes
or an empty list 
|#

(define (dup-helper attr1 attr2)
  (cond [(empty? attr1) '()]
        [(empty? attr2) '()]
        [(find-item attr2 (first attr1))
         (cons (first attr1) (dup-helper (rest attr1) attr2))]
        [else (dup-helper (rest attr1) attr2)]))

#|
A function that takes a list of lists of attributes and returns
attributes present in all lists
|#
(define (duplicates? attr-lst)
  (if (empty? (rest attr-lst))
      '()
       (remove-duplicates (flatten (append
        (map (lambda (attrs) (dup-helper  (first attr-lst) attrs)) (rest attr-lst))
        (duplicates? (rest attr-lst)))))))


#|
A function that takes:
   - a name
   - an attribute

and returns the attribute appened to the letter with a .
|#
(define (rename-attr attr name)
  (string-append name "." attr))

#|
A function that takes:
   - a list of attributes
   - a list of duplicate attributes
   - a name

and returns a new list with duplicate attributes renamed
|#

(define (rename-attr-list attr-lst dup-lst name)
  (if (empty? attr-lst)
      '()
      (cons (if (find-item dup-lst (first attr-lst))
                (rename-attr (first attr-lst) name)
                (first attr-lst))
            (rename-attr-list (rest attr-lst) dup-lst name)))
  )

#|
A function that takes:
  - a list of names
  - a list of attributes

and returns a list of attributes renamed with the corresponding name
|#

(define (rename-attrs name-lst attr-lst)
  (let ([dup-lst (duplicates? attr-lst)])
    (define (rename-helper name-lst attr-lst)
      (if (empty? attr-lst)
          '()
          (append (rename-attr-list (first attr-lst) dup-lst (first name-lst))
                  (rename-helper (rest name-lst) (rest attr-lst)))
          ))
    (rename-helper name-lst attr-lst)))

#|
A function that takes:
   - a list of tuples

and returns the cartesian product of all the tuples
|#

(define (multi-join tuples)
  (if (empty? (rest tuples))
      (first tuples)
      (cartesian-product (first tuples) (multi-join (rest tuples))))
  )

#|
A function that takes:
   - a list of tables
and returns the cartesian product and renamed attributes
|#

(define (multi-select table)
  (define table-attrs (map first (map rest table)))
  (define table-tuples (map rest (map rest table)))
  (define table-names (map first table))
  
  (cons (rename-attrs table-names table-attrs)
        (multi-join table-tuples))
  )


;WHERE Helpers

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#
(define (filter-table f table)
  (cons (attributes table)
        (filter f (tuples table))))

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replace-attr x attr-list)
  (lambda (tuple)
    (if (list-index attr-list x)
        (find-attr attr-list x tuple)
        x)))

#|
A function that takes:
  - a table
  - an attribute

   sorts the tuples in descending order according to the attribute
   and returns the ordered table
|#
(define (order-table attr table)
  (cons (attributes table)
        (sort (tuples table) > #:key attr)))


; Starter for Part 3; feel free to ignore!

; What should this macro do?
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     (lambda (tuple)
       (if (empty? tuple)
           tuple
           (let ([sub-expr (list ((replace expr table) tuple)
                              ...)])
             (apply (first sub-expr) (rest sub-expr)))))]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (lambda (tuple)
       (if (empty? tuple)
           tuple
           ((replace-attr atom table) tuple)))]))

;Macro to deal with mutiple table names
(define-syntax name-tables
  (syntax-rules ()
    [(name-tables [<table> <name>])
     (cons <name> <table>)]
    [(name-tables <entry> ...)
     (append (list (name-tables <entry>))
             ...)]
    ))


;WHERE Syntax
(define-syntax where
  (syntax-rules ()
    [(where <cond> <table>)
     (filter-table (replace <cond> (attributes <table>))
                   <table>)]
    ))

;ORDER BY Syntax
(define-syntax order
  (syntax-rules ()
    [(order <cond> <table>)
     (order-table (replace <cond> (attributes <table>))
                  <table>)]))

;SELECT Syntax
(define-syntax SELECT
  (syntax-rules (* FROM WHERE ORDER BY)
    
    ;* -> FROM <table> WHERE <cond> ORDER BY <order>
    [(SELECT <attrs> FROM <table> ... WHERE <cond> ORDER BY <order>)
     (SELECT <attrs> FROM
             (order <order>
                    (where <cond>
                           (SELECT * FROM <table> ...))))]
    
    
    ;* -> FROM <table> ORDER BY <cond>
    [(SELECT <attrs> FROM <table> ... ORDER BY <cond>)
     (SELECT <attrs> FROM
             (order <cond>
                    (SELECT * FROM <table> ...)))]


    ;* -> FROM <table> WHERE <cond>
    [(SELECT <attrs> FROM <table> ... WHERE <cond>)
     (SELECT <attrs> FROM (where <cond>
                           (SELECT * FROM <table> ...)))]
    
    ;* -> FROM
    [(SELECT * FROM <table>)
     <table>]
    
    ;<attr> -> FROM
    [(SELECT <attr> FROM <table>)
     (select-table <attr> <table>)]
    
    ;* -> FROM Multiple Tables
    [(SELECT * FROM <table1> <table2> ...)
     (multi-select (name-tables <table1> <table2> ...))]
    
    ;<attr> -> FROM Multiple Tables
    [(SELECT <attr> FROM <table1> <table2> ...)
     (select-table <attr> (multi-select (name-tables <table1> <table2> ...)))]
    ))




(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))

(define Sports
  '(("Name" "Sport")
    ("David" "ball")
    ("Paul" "swim")
    ("David" "fight")))

(define p-attrs (attributes Person))
(define p-tuples (tuples Person))

(define t-attrs (attributes Teaching))
(define t-tuples (tuples Teaching))

(define table (name-tables [Person "P"] [Teaching "T"] [Person "P1"]))
(define table-attrs (map first (map rest table)))
(define table-tuples (map rest (map rest table)))
(define table-names (map first table))
