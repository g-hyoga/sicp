#lang racket

(define (cons x y)
	(lambda (m) (m x y)))

(define (car z)
	(z (lambda (p q) p)))


(car (cons 1 2))
















