#lang racket

(define (make-rat n d)
	(let ((g (gcd n d)))
		(cons (/ n d) (/ d g))))

(define (print-rat x)
	(display (car x))
	(display "/")
	(display (cdr x))
	(newline))

(define (add-rat x y)
	(cons (+ (* (car x) (cdr y))
					 (* (car y) (cdr x)))
				(* (cdr x) (cdr y)))) 

(print-rat (cons 1 2))
















