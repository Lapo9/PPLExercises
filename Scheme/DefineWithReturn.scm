;06/07/2018

#lang racket

(provide defineWithReturnTest)
(provide deepen)

(define (deepen L)
  (foldl (lambda (x xs) (if (eqv? xs '()) (list x) (list xs x))) '() L)
)


(define-syntax define-with-return:
  (syntax-rules ()
  [
    (_ m (name param ...) body ...)
    (define (name param ...)
      (call/cc (lambda (cont)
        (let ([m cont])
          body ...
        )
      ))
    )
  ]
  )
)

(define (defineWithReturnTest)
  (define-with-return: return (f x)
    (define a 12)
    (return (+ x a))
    100
  )

  (f 3) ;should evaluate to 15
)