#lang racket

(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ (term a) result))))
	(iter a 0))


(define (cube n) (* n n n))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
	(sum cube a inc b))

(sum-cubes 1 10)









