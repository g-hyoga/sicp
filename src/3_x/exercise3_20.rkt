#lang planet neil/sicp

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 1)
(car x)



