#lang racket

(define (product term a next b)
	(if (> a b)
		1.0
		(* (term a)
			 (product term (next a) next b))))

(define (factorial a b)
	(define (square n) (* n n))
	(define (term n)
		(/ (* n (+ n 2)) (square (+ n 1))))
	(define (next n) (+ n 2))
	(product term a next b))

(* (factorial 2 1000) 4)









