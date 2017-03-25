#lang racket 
(require "package/math.rkt")

(define c (make-complex-from-real-imag 3 0))
(define r (make-real 7.0))
(define q (make-rational 2 1))
(define z (make-integer 3))
(define zero (make-integer 0))

(define p1 (make-poly 'x (list (list z z))))
(define p2 (make-poly 'x (list (list 10 0) (list 2 3) (list 0 4))))
(define p3 (make-poly 'x (list (list zero zero))))
;(=zero? p1)
(=zero? p3)

(add p1 p1)

