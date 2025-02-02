#lang racket
(require "package/get-put.rkt")
(require "package/tag.rkt")
(require "package/integer.rkt")
(require "package/real.rkt")
(require "package/rational.rkt")
(require "package/rectangular.rkt")
(require "package/polar.rkt")
(require "package/complex.rkt")

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags))) 
			(if proc
				(apply proc (map contents args))
				(if (= (length args) 2)
					(let ((type1 (car type-tags)) 
								(type2 (cadr type-tags))
								(a1 (car args))
								(a2 (cadr args)))
						(cond ((eq? type1 (higher type1 type2)) 
									 (apply-generic op a1 (raise a2)))
									((eq? type2 (higher type1 type2))
									 (apply-generic op (raise a1) a2))
									(else (error "No method for these types" (list op type-tags)))))
					(error "No method for these types" (list op type-tags)))))))

;;;body;;;
(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (make-scheme-number n)
	((get 'make 'scheme-number) n))
(define (make-integer i)
	((get 'make 'integer) i))
(define (make-real r)
	((get 'make 'real) r))
(define (make-rational n d)
	((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
	((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
	((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (drop x)
	(if (eq? 'integer (type-tag x))
		x
		(let ((p (project x)))
			(if (equ? x (raise p))
				(drop p)
				p))))

(define type-tower
	'(complex real rational integer))

(define (higher t1 t2)
	(define (iter t1 t2 type-tower)
		(cond ((eq? t1 t2) t1)
					((eq? t1 (car type-tower)) t1)
					((eq? t2 (car type-tower)) t2)
					(else (iter t1 t2 (cdr type-tower)))))
	(iter t1 t2 type-tower))

(define c (make-complex-from-real-imag 3 0))
(define r (make-real 7.0))
(define q (make-rational 2 1))
(define z (make-integer 3))

(drop r)
