#lang racket

(define (cont-frac n d k)
	(define (iter result i)
		(if (> 0 i)
			result
			(iter (/ (n i) (+ (d i) result)) (- i 1))))
	(iter 0 k))


(/ 1(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))



;answer
(define phi (/ (+ 1 (sqrt 5)) 2))
(+ 1 (/ 1 phi))


























