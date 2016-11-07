#lang racket
#| Assignment 2 - Classes

This file contains your work for Questions 1 and 2, extending the basic
class macro to include support for traits and some basic introspection.
|#
(provide class-meta class-trait)

; QUESTION 1 (metaprogramming).
(define-syntax class-meta
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(method <param> ...) <body>] ...)
     
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? "_attributes" msg)
                (map (lambda (id val)
                       (cons id (list val)))
                     (list (id->string <attr>) ...)
                     (list <attr> ...))]
               
               [(equal? "_methods" msg)
                (map (lambda (id val)
                       (cons id (list val)))
                     (list (id->string method ...))
                     (list (lambda (<param> ...) <body>))...)]
               
               [else "Unrecognizable Message"]
               )))]))

(class-meta Point-meta (x y)
            [(distance other-point)
             (let ([dx (- x (other-point "x"))]
                   [dy (- y (other-point "y"))])
               (sqrt (+ (* dx dx) (* dy dy))))])

(let ([p (Point-meta 2 3)])
  (p "_attributes"))

(let ([p (Point-meta 2 3)])
  (p "_methods"))


; QUESTION 2 (traits).
(define-syntax class-trait
  (syntax-rules ()
    
    ))

; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))
