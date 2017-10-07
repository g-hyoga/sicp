#lang racket

(define (product term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* (term a) result))))
	(iter a 1.0))

(define (factorial a b)
	(define (square n) (* n n))
	(define (term n)
		(/ (* n (+ n 2)) (square (+ n 1))))
	(define (next n) (+ n 2))
	(product term a next b))

(* (factorial 2 1000) 4)









