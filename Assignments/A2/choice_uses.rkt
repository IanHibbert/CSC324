#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#
(define (subsets lst)
  (parse (subset lst)))

(define (add-to-all lst item)
  (if (null? lst)
      empty
      (cons (cons item (car lst))
            (add-to-all (cdr lst) item))))
; subsets

(define (subset lst)
  (if (empty? lst)
      '(())
      (let ((rst (subset (cdr lst))))
        (append (add-to-all rst (car lst))
                rst))))

(define (parse lst)
  (if (empty? (rest lst))
      (first lst)
      (-< (first lst)
          (parse (rest lst)))))


(define (insert lst val)
  (if (empty? lst)
      '()
      (-< (cons val lst)
          (cons (first lst)
                (insert (rest lst) val)))))


; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
(define grid '((1 2 3 4)
               (3 4 1 2)
               (4 1 2 3)
               (2 3 4 1)))
(define grid1
  '((1 2 3 4)
    ("" "" 1 "")
    ("" "" 2 3)
    (2 "" "" 1)))

(define (sudoku-4 grid)
  (?- valid-grid (list (sub-< (first grid))
                       (sub-< (second grid))
                       (sub-< (third grid))
                       (sub-< (fourth grid))
                       )))


(define (sub-< lst)
  (cond [(empty? lst) '()]
        [(equal? "" (first lst)) (append (list (-< 1 2 3 4))
                                         (sub-< (rest lst)))]
        [else (cons (first lst)
                    (sub-< (rest lst)))]))


(define (valid-grid grid)
  (define row1 (first grid))
  (define row2 (second grid))
  (define row3 (third grid))
  (define row4 (fourth grid))
  
  (define col1 (map (lambda (row) (first row)) grid))
  (define col2 (map (lambda (row) (second row)) grid))
  (define col3 (map (lambda (row) (third row)) grid))
  (define col4 (map (lambda (row) (fourth row)) grid))
  
  (define q1 (list (first row1) (second row1) (first row2) (second row2)))
  (define q2 (list (third row1) (fourth row1) (third row2) (fourth row2)))
  (define q3 (list (first row3) (second row3) (first row4) (second row4)))
  (define q4 (list (third row3) (fourth row3) (third row4) (fourth row4)))
  
  (and (valid-entry row1)
       (valid-entry row2)
       (valid-entry row3)
       (valid-entry row4)
       (valid-entry col1)
       (valid-entry col2)
       (valid-entry col3)
       (valid-entry col4)
       (valid-entry q1)
       (valid-entry q2)
       (valid-entry q3)
       (valid-entry q4)
       )
  
  )

(define (valid-entry lst)
  (equal? (set 1 2 3 4) (list->set lst))
  )

(sudoku-4 grid)

; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define (foldlr func accum lst)
  (if (null? lst)
      accum
      (foldl func
             (func (car lst) accum) ; here's the change
             (cdr lst))))

(define-syntax fold-<
  (syntax-rules ()
    [(fold-< <combine> <init> <expr>)
       (foldl <combine> <init> (all <expr>))]
))
