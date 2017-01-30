#lang racket

(define (count-leaves x)
	(cond ((null? x) 0)
				((not (pair? x)) 1)
				(else (+ (count-leaves (car x))
								 (count-leaves (cdr x))))))

;;;;;;;;;;
(define (push a l)
	(if (null? l)
		(cons a l)
		(cons (car l) (push a (cdr l)))))

(define (deep-reverse l)
	(cond ((null? l) l)
				((pair? (car l)) (push (deep-reverse (car l))
															 (deep-reverse (cdr l))))
				(else (push (car l) (deep-reverse (cdr l))))))
;;;;;;;;;;

(define x (list (list 1 2) (list 3 4)))

x
(reverse x)
(deep-reverse x)


