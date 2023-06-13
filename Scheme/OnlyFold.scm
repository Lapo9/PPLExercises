;16/01/2019

#lang racket

(provide folder)

(define (folder . xs)
    (foldl (lambda (a as) (list a as)) (foldl (lambda (b bs) (cons b bs)) '() xs) xs)
)