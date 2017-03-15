#lang racket 
(require "package/math.rkt")

(define p1 (make-poly 'x (list (list 100 1) (list 2 2) (list 0 1))))
(define p2 (make-poly 'x (list (list 10 0) (list 2 3) (list 0 4))))
(define p3 (make-poly 'x (list (list 0 0))))
(=zero? p3)

(define c (make-complex-from-real-imag 3 0))
(define r (make-real 7.0))
(define q (make-rational 2 1))
(define z (make-integer 3))

(add p1 p2)

