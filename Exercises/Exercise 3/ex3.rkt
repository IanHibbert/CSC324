#| Exercise 5 - Practice with macros (due October 8, 11:50pm)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Racket libraries, unless explicitly told to.
- You may not use mutation or any iterative constructs (for/*).
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

Implement each of the following two macros.
|#
#lang racket

(provide flipped-and record)

#|
(flipped-and <arg1> <arg2> ... <argn>)
  Takes 0 or more arguments.
  Each argument is an expression that evaluates to a boolean value.

  Returns the AND of all the arguments (i.e., #t if all evaluate to true,
  and #f otherwise). The difference between 'flipped-and' and 'and' 
  is that this one evaluates arguments *right-to-left*, and short-circuits.

  When given 0 arguments, returns #t.

> (flipped-and (/ 1 0) #f)
#f
> (flipped-and)
#t
|#
(define-syntax flipped-and
  (syntax-rules ()
    
    
  ))

#|
(record <class> (<attr1> ...))
  <class> and <attr1> ... are all identifiers.

  This macro defines a class in the same way that the existing class macro
  does (the code is included below). However, it *also* defines convenience
  functions to access each of the class' instance attributes directly.

  These functions will have the same name as an attribute of the class,
  take an instance of the class as an argument, and then return the
  value of the corresponding attribute of that instance.
   
> (record Point (x y))
> (define p (Point 2 3))
> (p "x") ; old syntax still works
2
> (x p) ; new syntax
2

Note that you can define multiple functions in the body of a macro simply by
putting them one after the other inside a "begin" expression.
The macro will expand into *all* of the definitions, and they are all evaluated
inside the "begin".

(begin
  (define x 10)
  (define y 100)
  (define z 1000))

|#
(define-syntax record
  (syntax-rules ()
    
  ))

; For your reference, the class macro from lecture notes.
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>]
       ...
       )
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Attribute error"])))]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>) (symbol->string (quote <id>))]))
