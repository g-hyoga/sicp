#lang racket 

(define (fib n) 
	(if (>= 2 n) 
		1 
		(+ (fib (- n 1)) (fib (- n 2)))))


(fib 42)

