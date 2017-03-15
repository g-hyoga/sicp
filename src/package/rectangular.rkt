#lang racket
(require "tag.rkt")
(require "get-put.rkt")

(define (install-rectangular-package)
	; internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (square x) (* x x))
	(define (magnitude z)
		(sqrt (+ (square (real-part z))
						 (square (imag-part z)))))
	(define (angle z)
		(atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
		(cons (* r (cos a)) (* r (sin a))))
	(define (=zero? z)
		(and (= (real-part z) 0)
				 (= (imag-part z) 0)))

	; interface to the rest of the system
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part) 
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag 'rectangular
			 (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular
			 (lambda (r a) (tag (make-from-mag-ang r a))))
	(put '=zero? '(rectagular) (lambda (z) (=zero? z)))
	'done)

(provide install-rectangular-package)


