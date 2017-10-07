#lang racket

(define (compose f g)
	(lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeat f n)
	(define (iter result n)
		(if (= n 1)
			result
			(iter (compose f result) (- n 1))))
	(iter f n))

((repeat square 2) 5)















