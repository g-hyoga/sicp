#lang racket

(define (exp a b)
	(define (iter result b)
		(if (= b 0)
			result
			(iter (* result a) (- b 1))))
	(iter a (- b 1)))

(define (log a b)
	(define (iter result b)
		(if (= (remainder b a) 0)
			(iter (+ result 1) (/ b a))
			result))
	(iter 0 b))

(define (cons a b)
	(* (exp 2 a) (exp 3 b)))

(define (car x)
	(define (iter result)
		(if (= (remainder result 3) 0)
			(iter (/ result 3))
			(log 2 result)))
	(iter x))

(define (cdr x)
	(define (iter result)
		(if (= (remainder result 2) 0)
			(iter (/ result 2))
			(log 3 result)))
	(iter x))


(car (cons 3 2))
(cdr (cons 3 2))







