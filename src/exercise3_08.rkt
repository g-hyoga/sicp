#lang racket

(define func 
	(let ((zero? #f))
		(define (dispatch n)
			(if (= n 0)
				(begin (set! zero? #t) n)
				(if zero? 
					(begin (set! zero? #f) 0) 
					n)))
		dispatch))
(define f func)

(+ (f 0) (f 1))
(+ (f 1) (f 0))

