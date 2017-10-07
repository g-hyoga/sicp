#lang racket

(define (count-leaves x)
	(cond ((null? x) 0)
				((not (pair? x)) 1)
				(else (+ (count-leaves (car x))
								 (count-leaves (cdr x))))))

(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))

;;;;;;;;;;;;;;
(define (fringe tree)
	(cond ((null? tree) null)
				))

;;;;;;;;;;;;;;

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))
