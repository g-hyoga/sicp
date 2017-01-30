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

;;;;;;;;;;;
#;(define (square-list items)
(if (null? items)
	null
	(cons (* (car items) (car items))
				(square-list (cdr items)))))

(define (square-list items)
	(map (lambda (x) (* x x)) items))
;;;;;;;;;;;


(define test (list 1 2 3 4))
(square-list test)
