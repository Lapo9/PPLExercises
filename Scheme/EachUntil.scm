;07/02/2020

#lang racket

(provide eachUntil)

(define-syntax each-until
  (syntax-rules(in until :)
  [
    (_ var in l until pred : body ...)
    ;here we save the continuation
    (call/cc (lambda (cont)
      ;here we do the actual loop
      (for-each (lambda(x)
        (let ((var x))
          (if pred 
            (cont) ;call the continuation, which would proceed after the loop
            (begin body ...) ;execute bodies
          )
        )) l
      ))
    )
  ]
  )
)

(define (eachUntil)
  (each-until y in '(1 2 5 2 9 5 7)
    until (> y 6):
    (display (* y 100))
    (newline)
  )
)
  