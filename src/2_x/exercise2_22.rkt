#lang racket

(define (scale-list items factor)
	(if (null? items)
		null
		(cons (* (car items) factor)
					(scale-list (cdr items)
											factor))))

(define (map proc items)
	(if (null? items)
		null
		(cons (proc (car items))
					(map proc (cdr items)))))

(define square (lambda (x) (* x x)))

(define (square-list items)
	(define (iter things answer)
		(if (null? things)
			(reverse answer)
			(iter (cdr things)
						(cons (square (car things))
									answer))))
	(iter items null))


(define test (list 1 2 3 4))
(square-list test)
