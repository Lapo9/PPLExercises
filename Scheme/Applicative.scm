;15/01/2020

#lang racket

(provide vector-<*>)

(define (vector-foldr combine acc vec)
  (let loop ((i (- (vector-length vec) 1)))
    (set! acc (combine (vector-ref vec i) acc))
    (if (= i 0) acc (loop (- i 1)))
  )
)

(define (vector-foldl combine acc vec)
  (let loop ((i 0) (top (vector-length vec)))
    (set! acc (combine (vector-ref vec i) acc))
    (if (= i (- top 1)) acc (loop (+ i 1) top))
  )
)

(define (vector-pure x) (vector x))

(define (flatten vecOfVecs)
  (vector-foldr vector-append #() vecOfVecs)
)

(define (vector-<*> funcsVec itemsVec)
  (flatten (vector-map 
    (lambda (func) 
      (vector-map func itemsVec)
    ) 
    funcsVec)
  )
)