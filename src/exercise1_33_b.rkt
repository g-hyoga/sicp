#lang racket

(define (square x) (* x x))

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(define (filtered-accumulate term a next b combiner null-value filter)
	(define (iter a result)
		(if (> a b)
			result
			(if (filter a)
				(iter (next a) (combiner (term a) result))
				(iter (next a) result))))
	(iter a null-value))	

(define (product-m-prime b)
	(define (inc x) (+ x 1))
	(define (tmp x) x)
	(define (filter x) (= (gcd x b) 1))
	(filtered-accumulate tmp 1 inc b * 1 filter))

(product-m-prime 10)








