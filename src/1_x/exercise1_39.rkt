#lang racket

(define (cont-frac n d k)
	(define (iter result i)
		(if (= 0 i)
			result
			(iter (/ (n i) (+ (d i) result)) (- i 1))))
	(iter 0 k))


(define (tan-cf x k)
	(cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
						 (lambda (i) (- (* 2 i) 1))
						 k))

(tan-cf (/ 3.141592 4) 10000)

























