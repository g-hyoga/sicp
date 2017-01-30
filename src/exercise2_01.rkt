#lang racket

(define (make-rat n d)
	(let ((g (gcd n d)))
		(if (< d 0)
			(cons (/ (* -1 n) g) (/ (* -1 d) g))
			(cons (/ n g) (/ d g)))))

(define (print-rat x)
	(display (car x))
	(display "/")
	(display (cdr x))
	(newline))

(print-rat (make-rat 1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat -1 -2))



