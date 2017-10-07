#lang racket

(define pi 3.14159)
(define radius 10) 

(define circumference (* 2 pi radius))

(define (square x) (* x x))

(define (sum-of-squares x y) 
	(+ (square x) (square y)))

(define (f a)
	(sum-of-squares (+ a 1) (* a 2)))
 
(define (abs x)
  (cond ((< x 0) (- x))
				(else x)))
      





