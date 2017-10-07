#lang racket

(define (cube a) (* a a a))

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			 (sum term (next a) next b))))

(define (intergral f a b n)
	(define h (/ (- b a) n))
	(define (next a) (+ a h h))
	(* (/ h 3.0)
		 (- (+ (* 4.0 (sum f (+ a h) next b))
					 (* 2.0 (sum f a next b)))
				(f a)
				(f b))))

(intergral cube 0 1 10)



