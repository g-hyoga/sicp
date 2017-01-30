#lang racket


(define (fib-iter a b n) 
	(if (>= 0 (- n 2))
			 a
			 (fib-iter (+ a b) a (- n 1))))

(define (fib n) (fib-iter 1 1 n))

(fib 100)




