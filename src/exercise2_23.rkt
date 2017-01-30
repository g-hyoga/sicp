
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

(define (square-list items)
	(map (lambda (x) (* x x)) items))

;;;;;;;;;;;;
(define (for-each proc items)
	(cond ((null? items) null)
				(else (proc (car items)) (for-each proc (cdr items)))))
;;;;;;;;;;;;

(define test (list 1 2 3 4))

(for-each (lambda (x)
						(display x)
						(newline))
					(list 57 321 88))
