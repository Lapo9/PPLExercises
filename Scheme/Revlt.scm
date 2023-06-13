;05/09/2018

#lang racket

(provide revlt)

(define (revlt L M N)
  (define (truncate L N)
    (define (truncateImpl L N n)
      (if (= n N) 
        (cons (car L) '())
        (cons (car L) (truncateImpl (cdr L) N (+ n 1)))
      )
    )

    (truncateImpl L N 1)
  )
  
  (let ([o (min (length L) (length M) (length N))])
    (foldl 
      (lambda (l m n acc)(cons (vector l m n) acc)) 
      '()
      (truncate L o)
      (truncate M o)
      (truncate N o)
    )
  )
)