#lang racket

(define (fib-iter a b n) 
	(if (>= 0 (- n 2))
		a
		(fib-iter (+ a b) a (- n 1))))

(define (fib_i n) (fib-iter 1 1 n))


(define (fib_r n) 
	(if (>= 2 n) 
		1 
		(+ (fib_r (- n 1)) (fib_r (- n 2)))))


(fib_r 42)







