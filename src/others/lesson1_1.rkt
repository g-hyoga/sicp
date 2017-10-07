#lang racket

(+ 137 349)

(+ 21 35 12 7)

(+ (* 3 5) (- 10 6))

(+ (* 3 (+ (* 2 4)
					 (+ 3 5))) 
	 (+ (- 10 7)
			6))

(define size 2)
size

(* 5 size)

(define pi 3.14159)
(define radius 10) 

(* pi (* radius radius))

(define circumference (* 2 pi radius))

circumference

(define (square x) (* x x))

(square 21)

(define (sum-of-squares x y) 
	(+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
	(sum-of-squares (+ a 1) (* a 2)))

(f 5)
(sum-of-squares 6 10)
(+ (square 6) (square 10))
(+ 36 100)
136

#;(define (abs x)
	(cond ((> x 0) x)
				((< x 0) (- x))
				((= x 0) 0)))

#;(define (abs x)
  (cond ((< x 0) (- x))
				(else x)))

(define (abs x) 
	(if (< x 0) (- x) x))

(abs -3)

(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
	(average guess (/ x guess)))

(define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
	(if (good-enough? guess x) 
		guess
		(sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt 1000000)

(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
				(else else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (sqrt-iter2 guess x)
	(new-if (good-enough? guess x) 
		guess
		(sqrt-iter2 (improve guess x) x)))

(define (sqrt2 x) (sqrt-iter2 1.0 x))

(sqrt2 2)







