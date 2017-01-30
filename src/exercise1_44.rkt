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

(define (smooth f dx n)
	(define (iter )))

(define (smooth-1 f dx)
	(lambda (x) (average f x dx)))

(define (average f x dx)
	(/ (+ (f x)
				(f (+ x dx))
				(f (- x dx)))
		 3))


((smooth square 0.1 3) 2)












