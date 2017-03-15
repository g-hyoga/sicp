#lang racket
(require "tag.rkt")
(require "get-put.rkt")

(define (install-real-package)
	(define (tag x) (attach-tag 'real x))
	;;;;;;;;;;;;;;;;;;;
	(define (decimal-place x)
		(define (iter x result)
			(if (= (quotient x 10) 0)
				result
				(iter (quotient x 10) (+ result 1))))
		(iter x 0))
	(define (project x)
		((get 'make 'rational) x
													 (expt 10 (decimal-place x))))

	(put 'add '(real real)
			 (lambda (x y) (tag (+ x y))))
	(put 'sub '(real real)
			 (lambda (x y) (tag (- x y))))
	(put 'mul '(real real)
			 (lambda (x y) (tag (* x y))))
	(put 'div '(real real)
			 (lambda (x y) (tag (/ x y))))
	(put 'make 'real (lambda (x) (tag (* 1.0 x))))
	(put 'equ? 'real (lambda (x y) (= x y)))
	(put '=zero? '(real) (lambda (x) (= x 0)))
	(put 'raise '(real)
			 (lambda (x) ((get 'make-from-real-imag 'complex) x 0)))
	(put 'project '(real) project)
	'done)

(provide install-real-package)




