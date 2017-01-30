#lang racket

(define (double p)
	(lambda (x) (p (p x))))

(define (inc x)
	(+ x 1))

(((double (double double)) inc) 5)





