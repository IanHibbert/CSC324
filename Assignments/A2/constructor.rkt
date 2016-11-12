#lang racket
#|
This macro binds every attribute in the init function
and binds the attribute to the corresponding expression.
The only attributes that can be accessed are the ones the have the self prefix.
If a user tries to access a variable that is not associated with the class using "self"
then an "Unrecognized message" error is returne.
|#
(define-syntax class
  (syntax-rules (: def __init__  = )
    [(class <Class> :
       def __init__ (self <param> ...) :
       (<attr> = <expr>)
       ...
       )
     (define (<Class> <param> ...)
       (define <attr> <expr>)
       ...
       (lambda (msg)
         (cond [(equal? msg (self? (id->string <attr>))) <expr>]
               ...
               
               [else "Unrecognized message!"]
               )))]))


(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))

(define-syntax self?
  (syntax-rules ()
    [(self <id>)
     (if (regexp-match "self." <id>)
         (substring <id> 5)
         (void))]))

(class MyClass :
  def __init__ (self a b) :
  (r = (f a))
  (self.x = (f a))
  (self.y = `(,b 100 ,r))
  (z = "you are cool")
  )

(define (f r)
  (+ 5 r))

(let ([c1 (MyClass 1 2)])
  (c1 "x"))

(let ([c1 (MyClass 1 2)])
  (c1 "y"))

(let ([c1 (MyClass 1 2)])
  (c1 "z"))



; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
#|
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
|#