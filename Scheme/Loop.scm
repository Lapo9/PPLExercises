#lang racket

(provide loop)
(provide forEachLoop)

(define (loop) 
  (let label ((x 0) (y 100))
    (when (< x 10)
      (display (+ x y))
      (display ", ")
      (label (+ x 1) y)
    )
  )
)

(define (forEachLoop)
  (for-each (lambda (x)
              (+ x 1)
              (display x)
              (display ", ")
            ) '(2 3 7)
  )
)