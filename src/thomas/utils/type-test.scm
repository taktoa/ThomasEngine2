;;; File: type-test.scm
;;
;;; License:
;; Copyright © 2015 Remy Goldschmidt <taktoa@gmail.com>
;;
;; This file is part of ThomasEngine2.
;;
;; ThomasEngine2 is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; ThomasEngine2 is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ThomasEngine. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Author:     Remy Goldschmidt <taktoa@gmail.com>
;;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;;; Created:    August 30th, 2015
;;
;;; Homepage:   https://github.com/taktoa/ThomasEngine2
;;
;;; Commentary:
;; This is a test case for the type system.
;;
;;; Code:

(define-module (thomas utils type-test)
  #:version    (0 0 1)
  #:use-module (thomas utils type-system)
  #:use-module (sly)
  #:use-module (srfi  srfi-64)
  #:export     (run-type-tests))

(define type-check-tests '())

(define-syntax type-check-test
  (syntax-rules ()
    ((_ name message sexpr expected-tenv)
     (let ([observed-tenv (type-check sexpr)])
       (test-begin #`#,(symbol->string name))
       (for-each (λ (x) (test-equal observed-tenv
                                    expected-tenv)) sexpr)
       (test-end   #`#,(symbol->string name))))))

;; ;; Initialize and give a name to a simple testsuite.
;; (test-begin "vec-test")
;; (define v (make-vector 5 99))
;; ;; Require that an expression evaluate to true.
;; (test-assert (vector? v))
;; ;; Test that an expression is eqv? to some other expression.
;; (test-eqv 99 (vector-ref v 2))
;; (vector-set! v 2 7)
;; (test-eqv 7 (vector-ref v 2))
;; ;; Finish the testsuite, and report results.
;; (test-end "vec-test")

(type-check-test boolean-lits
 "Testing boolean literals."
 '((define test-1 #t)
   (define test-2 #f))
 '((test-1 . Bool)
   (test-2 . Bool)))

(type-check-test numeric-lits
 "Testing numeric literals."
 '((define test-1 5)
   (define test-2 5/4)
   (define test-3 5.0)
   (define test-4 5.4)
   (define test-5 4+3i))
 '((test-1 . Exact   Integer)
   (test-2 . Exact   Rational)
   (test-3 . Inexact Integer)
   (test-4 . Inexact Rational)
   (test-5 . Inexact Complex)))

(type-check-test integer-arith
 "Testing integer arithmetic."
 '((define test-1 (+ 5 4))
   (define test-2 (* 5 4))
   (define test-3 (- 5 4)))
 '((test-1 . Exact   Integer)
   (test-2 . Exact   Integer)
   (test-3 . Exact   Integer)))

(type-check-test rational-arith
 "Testing rational arithmetic."
 '((define test-1 (/ 5 4)))
 '((test-1 . Exact   Rational)))

(type-check-test complex-arith
 "Testing complex arithmetic."
 '((define test-1 (+ 5+4i 2+3i))
   (define test-2 (* 5+4i 2+3i))
   (define test-3 (- 5+4i 2+3i))
   (define test-4 (/ 5+4i 2+3i)))
 '((test-1 . Inexact Complex)
   (test-2 . Inexact Complex)))

(type-check-test conditionals
 "Testing conditional."
 '((define test-1 (if #t 5 4)))
 '((test-1 . Int)))
