;08/02/2019

#lang racket

(provide multiMerge)

(define (multiMerge l1 . ls)
  (define (sort L out)
    (let ([m (apply min L)] [newL '()])
      (set! out (append out (list m))) ;add m to out
      
      ;remove m from L (basically keeps adding L elements to newL, when it finds the first element == m, skip it and add all the remaining elements after it)
      (let loop ([remaining L])
        (if (= (car remaining) m)
          (set! newL (append newL (cdr remaining)))
          (begin
            (set! newL (append newL (list (car remaining))))
            (loop (cdr remaining))
          )
        )
      )
      
      (if (eqv? newL '()) out (sort newL out)) ;recursive call (if we don't have new elements, return)
    )
  )

  ;create a big list formed by merging all the lists (l1 + l2 + l3 + ...), and sort it
  (let ([L (append l1 (foldr (lambda (l acc) (append l acc)) '() ls))])
    (sort L '())
  )
)