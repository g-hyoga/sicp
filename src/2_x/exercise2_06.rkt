#lang racket

(define zero (lamda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f (x)))))
(define two (lambda (f) (lambda (x) (f (f (x))))))

(define (mul n m)
	(lambda (f) (n (m (f)))))

(define (add n m)
	(lambda (f) (lambda (x) ((n f) ((m f) (x))))))






