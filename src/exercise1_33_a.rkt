#lang racket

(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
	(= (smallest-divisor n) n))

(define (filtered-accumulate term a next b combiner null-value filter)
	(define (iter a result)
		(if (> a b)
			result
			(if (filter a)
				(iter (next a) (combiner (term a) result))
				(iter (next a) result))))
	(iter a null-value))	

(define (sum-prime a b)
	(define (inc x) (+ x 1))
	(filtered-accumulate square a inc b + 0 prime?))

(sum-prime 0 10)







