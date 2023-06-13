#lang racket

(provide continuationTest1)

(define (continuationTest1)
  (display (+ 3 (call/cc (lambda (cont) 
    (for-each (lambda (x)
      (when (< x 0) (cont x)))
      '(2 4 -5 -2 9) ;when you meet a negative element in this list, call the continuation (basically restart execution from after (+ 3 ?))
    )
    10)) ;if no element was negative, call/cc will return this value
  1000))
)