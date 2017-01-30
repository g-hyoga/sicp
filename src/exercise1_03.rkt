#lang racket

(define (square x) (* x x))

(define (sum-of-squares a b) 
	(+ (square a) (square b)))

#;(define (sum-of-squares-of-two-larger-num a b c)
	(cond ((and (>= a b) (>= a c)) (if (>= b c) (sum-of-squares a b) (sum-of-squares a c)))
				((and (>= b c) (>= b a)) (if (>= c a) (sum-of-squares b c) (sum-of-squares b a)))
 				((and (>= c a) (>= c b)) (if (>= a b) (sum-of-squares c a) (sum-of-squares c b)))))

(define (sum-of-squares-of-two-larger-num a b c) 
	(if (and (>= a b) (>= a c)) 
		(if (>= b c)
			(sum-of-squares a b)
			(sum-of-squares c a))
		(sum-of-squares-of-two-larger-num b c a)))



(sum-of-squares-of-two-larger-num 1 2 3)











