#| Sample tests for Exercise 3 (part 1).
Note the use of the "plai" language to use a more robust
testing framework.

All test expressions are in the form (test <actual> <expected>).

Note: these tests are only a subset of the tests we will run;
you are strongly advised to add to them on your own.

**WARNING**: because we're using macros here, there will be a pre-runtime
error if your macros do not implement the required syntax, causing
no tests to be run. (You'll see a "bad syntax" error message.)
Please make sure to test your code thoroughly before submitting!
|#
#lang plai
(abridged-test-output #t)

(require "ex3.rkt")

; Tests for flipped-and
(test (flipped-and)
      #t)

(test (flipped-and #t)
      #t)

(test (flipped-and #t (> 1 2) (empty? '()))
      #f)

(test (flipped-and (/ 1 0) #f)
      #f)

; Tests for record
(record Point (x y))

(test (let ([p (Point 2 3)])
        (p "x"))
      2)

(test (let ([p (Point 2 3)])
        (x p))
      2)
