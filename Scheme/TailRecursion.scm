;24/07/2019

#lang racket

(provide tailRecursionTest)

(define (tailRecursionTest L x y)
  (define (tailRecImpl L x y out)
    (if (eqv? L '()) out
      (let ([newOut out] [n (car L)] [A (car out)] [B (car (cdr out))] [C (car (cdr (cdr out)))]) 
        (begin
          (cond
            [(< n (min x y)) (set! newOut (list (cons n A) B C))]
            [(> n (max x y)) (set! newOut (list A B (cons n C)))]
            [else (set! newOut (list A (cons n B) C))]
          )
          (tailRecImpl (cdr L) x y newOut)
        )
      )
    )
  )
  (tailRecImpl L x y (list '() '() '()))
)