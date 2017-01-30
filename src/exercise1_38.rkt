#lang racket

(define (cont-frac n d k)
	(define (iter result i)
		(if (= 0 i)
			result
			(iter (/ (n i) (+ (d i) result)) (- i 1))))
	(iter 0 k))

(define (d i)
	(define (three-multi? x) (= (remainder x 3) 0))
	(if (three-multi? (+ i 1)) 
			(* 2 (/ (+ i 1) 3))
			1))

(+ (cont-frac (lambda (i) 1.0) d 1000) 2)




























