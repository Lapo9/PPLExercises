#lang racket

(provide count)

(define (count start)
  (let ((cur start))
    (lambda (increment)
      (set! cur (+ cur increment))
      (display cur)
      (newline)
    )
  )
)