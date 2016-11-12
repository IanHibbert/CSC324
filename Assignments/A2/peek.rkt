#| Choice Implementation

This is a cleaned-up version of the code from lecture.
While you are responsible for both understanding the
implementation and public API, only the latter is
required for this assignment.

|#
#lang racket
(provide -< next all clear ?-)

#|
(peek)
Returns representation of choices stored on stack of choices
|#
(define (peek)
  (if (empty? peeks)
      "false."
      (first peeks)))

#|
(-< <expr> ...)
  Each <expr> is an arbitrary Racket expression.

  Evaluates and returns the first <expr>.
  If there is more than one argument, stores a choice point
  which resumes the program at where the (-< ...) is used,
  but with the remaining choices.
|#
(define-syntax -<
  (syntax-rules ()
    ; When there is only one option, return it.
    [(-< <expr1>) <expr1>]
    ; When there is more than one, return the first and store the rest.
    [(-< <expr1> <expr2> ...)
     (begin
       (add-peek! `(-< ,<expr2> ...)) ; CHANGED: add quote of next continuation to stack
       (let/cc cont
         ; Push a new choice onto choices.
         (add-choice! (lambda () (cont (-< <expr2> ...))))
         <expr1>)
       )]))


#|
(next)

  Backtracks to the most recently stored choice point and
  resume program execution from there, or returns "false."
  if there are no choice points stored.
> (-< 1 2 3)
1
> (next)
2
> (next)
3
> (next)
"false."
|#
(define (next)
  ; Check if there are any remaining choices
  (if (empty? choices)
      "false."
      (begin
        (get-peek!) ; CHANGED: sets next peek value in stack
        
        ; Notice that it's ((get-choice!)) and not (get-choice!).
        ; What's the difference?
        ((get-choice!)))))


#|
(all <expr>)
  <expr> is a Racket expression, possibly containing choice points.
  
  Returns a list of all possible outcomes of evaluating <expr>
  (i.e., making all possible combinations of the choices).
|#
(define-syntax all
  (syntax-rules ()
    [(all <expr>)
     (let* (
            ; The list to store all the choices. Because macros are hygienic,
            ; we don't get name conflicts if we use all multiple times.
            [all-results '()]
            ; A helper function which mutates all-results.
            [all-helper 
             (lambda (e)
               (set! all-results (cons e all-results))
               ; If (next) calls a continuation, the last expression
               ; (reverse choices) will not execute (and hence not be
               ; returned. But if (next) returns "false." then the last
               ; expression does get executed.
               (next)
               (reverse all-results))])
       (all-helper <expr>))]))

#|
(clear)

  Remove all choice points from stack. Used for testing purposes.
|#
(define (clear)
  (set! choices '()))


#|
(?- pred expr)
  pred: a unary predicate (i.e., boolean function)
  expr: a choice expression

  Returns a choice from 'expr' which satisfies 'pred'.
  Calling 'next' will return all possible choices
  that satisfy 'pred'.

  
|#
(define (?- pred expr)
  (if (pred expr)
      expr
      ; If the predicate fails, try the next choice.
      (next)))


;------------------------------------------------------------------------------
; Private values for managing the stack of choices.
;------------------------------------------------------------------------------

;CHANGES BEGIN

;The stack representing peek
(define peeks '())

; "Push": add a peek to the peeks stack.
(define (add-peek! choice)
  (set! peeks
        (cons choice peeks)))

; "Pop": remove and return first choice from
; the peeks stack.
(define (get-peek!)
  (set! peeks (rest peeks))
  )

;END OF CHANGES

; The stack of choice points, represented as a list.
(define choices '())

; "Push": add a choice to the choices stack.
(define (add-choice! choice)
  (set! choices
        (cons choice choices)))

; "Pop": remove and return first choice from
; the choices stack.
(define (get-choice!)
  (let ([choice (first choices)])
    (set! choices (rest choices))
    choice))
