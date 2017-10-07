#lang racket

(define (accumulate term a next b combiner null-value)
	(define (iter a result)
		(if (> a b)
			null-value (iter (next a) (combiner (term a) result))))
	(iter a null-value))

(define (product term a next b)
	(accumulate term a next b * 1.0))

(define (factorial a b)
	(define (square n) (* n n))
	(define (term n)
		(/ (* n (+ n 2)) (square (+ n 1))))
	(define (next n) (+ n 2))
	(product term a next b))

(* (factorial 2 1000) 4)






