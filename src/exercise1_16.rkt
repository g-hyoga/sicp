#lang racket

(define (even? n)
	(= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-iter base counter product)
	(cond ((= counter 0) product)
				((even? counter) (fast-iter (square base) (/ counter 2) product))
				(else (fast-iter base (- counter 1) (* base product)))))

(define (fast-expt base n)
	(fast-iter base n 1))

(fast-expt 2 10)








