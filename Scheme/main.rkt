#lang racket

(require "Loop.scm")
(require "MinInList.scm")
(require "Closure.scm")
(require "WhileThenMacro.scm")
(require "ContinuationTests.scm")
(require "EachUntil.scm")
(require "Applicative.scm")
(require "TailRecursion.scm")
(require "ClosuresOop.scm")
(require "MultiMerge.scm")
(require "OnlyFold.scm")
(require "Revlt.scm")
(require "DefineWithReturn.scm")


(let ([program "ConsTest"]) (cond 
  [(eqv? program "Loop") (forEachLoop)]
  [(eqv? program "MinInList") (display (minInList '(5 2 6 8 1 7 1)))]
  [(eqv? program "Count") (let ((from6 (count 6))) (from6 2) (from6 5))]
  [(eqv? program "WhileThenMacro") (testWhileThenMacro)]
  [(eqv? program "ContinuationTests") (continuationTest1)]
  [(eqv? program "EachUntil") (eachUntil)]
  [(eqv? program "Applicative") (display (vector-<*> 
                                  (vector (lambda(x)(+ x 3)) (lambda(x)(* x 100)))
                                  #(2 5 3 8 9)
                                ))]
  [(eqv? program "TailRecursionTest") (display (tailRecursionTest '(5 1 2 11 7 9 18 2 10) 3 8))]
  [(eqv? program "ClosuresOopTest") (closuresOopTest)]
  [(eqv? program "MultiMerge") (display (multiMerge '(3 6 9) '(1 2 4 8 10) '(6)))]
  [(eqv? program "OnlyFold") (folder 1 2 3 4 5 6)]
  [(eqv? program "Revlt") (revlt '(11 12 13 14 15) '(21 22 23) '(31 32 33))]
  [(eqv? program "DefineWithReturn") (begin (display (defineWithReturnTest)) (newline) (display (deepen '(1 2 3 4 5 6))))]
  [(eqv? program "ConsTest") (begin
                                (display (cons 3 '[])) (newline)
                                (display (cons '[] 3)) (newline)
                                (display (cons 3 4)) (newline)
                                (display (cons '[3] '[])) (newline)
                                (display (cons '[3] 4)) (newline)
                                (display (cons '[1 2 3] '[4 5 6])) (newline)
                              )]

  [else (display "Program ") (display program) (display " doesn't exist")]
))