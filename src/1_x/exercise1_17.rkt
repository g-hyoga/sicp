#lang racket

(define (double n)
	(+ n n))

(define (halve n)
	(/ n 2))

(define (even? n)
	(= (remainder n 2) 0))

(define (iter a b n)
	(cond ((= n 0) a)
				((even? n) (iter a (double b) (halve n)))
				(else (iter (+ a b) b (- n 1)))))

(define (* b n)
	(iter 0 b n))

(* 8 3)
