#lang racket
;(require scheme/mpair)
(require r5rs)

(define (append x y)
	(if (null? x)
		y
		(cons (car x) (append (cdr x) y))))

(define (append! x y)
	(set-cdr! (last-pair x) y)
	x)

(define (last-pair x)
	(if (null? (cdr x)) 
		x 
		(last-pair (cdr x))))

(define (mystery x)
	(define (loop x y)
		(if (null? x)
			y
			(let ((temp (cdr x)))
				(set-cdr! x y)
				(loop temp x))))
	(loop x '()))

(define v (list 'a 'b 'c 'd))

(mystery v)
