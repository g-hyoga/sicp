#lang planet neil/sicp

(define (ripple-carry-adder as bs ss c-out)
	(cond ((null? as)) 'ok)
	(else (let ((c-in (make-wire)))
					(full-adder (car as) (car bs) c-in (car ss) c-out))
				(ripple-carry-adder (cdr as) (cdr bs) (cdr ss) c-in))

