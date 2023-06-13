#lang racket

(provide testWhileThenMacro)

; just a toy exapmle to practice with more than one pattern
(define-syntax WHILE
  (syntax-rules(DO THEN)
  [
    (_ condition DO body ...)
    (let loop()
      (display "Version 1 ")
      (when condition
        (begin
          body ...
          (loop)
        )
      )
    )
  ]
  [
    (_ condition (DO body1) (THEN body) ...)
    (let loop()
      (display "Version 2 ")
      (when condition
        (begin
          body1
          body ...
          (loop)
        )
      )
    )
  ]  
  )
)



(define (testWhileThenMacro)
  (let((x 10) (y 20))
    (WHILE (< x y)
      (DO (display x))
      (THEN (newline))
      (THEN (set! x (+ x 1)))
    )
  )  
)