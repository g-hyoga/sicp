#lang racket
 
(define (square x) (* x x))

(define (abs x) 
	(if (< x 0) (- x) x))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
	(average guess (/ x guess)))

(define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))

(define (improved-good-enough? guess before-guess x) 
	((> (abs ((square before-guess) (square guess))) 0.001))) 

(define (sqrt-iter guess x)
	(define before-guess guess)
	(if (improved-good-enough? guess before-guess x) 
		guess
		(sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))

(display "ok : ")
(sqrt 2)

(display "no : ")
(sqrt 0.0000003)






