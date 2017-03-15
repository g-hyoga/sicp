#lang racket
(require "tag.rkt")
(require "get-put.rkt")

(define (install-integer-package)
	(define (tag x) (attach-tag 'integer x))
	(define (make x) (tag (exact-floor x)))
	(put 'add '(integer integer)
			 (lambda (x y) (make (+ x y))))
	(put 'sub '(integer integer)
			 (lambda (x y) (make (- x y))))
	(put 'mul '(integer integer)
			 (lambda (x y) (make (* x y))))
	(put 'div '(integer integer)
			 (lambda (x y) (make (/ x y))))
	(put 'make 'integer make)
	(put 'equ? 'integer (lambda (x y) (= x y)))
	(put '=zero? '(integer) (lambda (x) (= x 0)))
	(put 'raise '(integer) (lambda (x) ((get 'make 'rational) x 1)))
	'done)

(provide install-integer-package)



