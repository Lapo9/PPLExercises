#lang racket

(provide minInList)

(define (minInList l)
  (if (eqv? (cdr l) '()) 
    (car l) ;list of 1 element
    (let ((first (car l)) (second (car (cdr l))))
    (cond
      ((eqv? (cdr (cdr l)) '()) (min first second)) ;list of 2 elements (base case)
      ((< first second) (minInList (append (list first) (cdr (cdr l))))) ;remove 2nd element and recurse
      (else (minInList (cdr l))) ;remove 1st element and recurse
    ))
  )
)