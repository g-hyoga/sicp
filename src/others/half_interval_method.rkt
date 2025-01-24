#lang racket

(define (search f neg-point pos-point)
	(define (average a b) (/ (+ a b) 2))
	(let ((midpoint (average neg-point pos-point)))
		(if (close-enogh? neg-point pos-point)
			midpoint
			(let ((test-value (midpoint)))
				(cond ((positive? test-value) (search f neg-point midpoint))
							((negative? test-value) (search f midpoint pos-point))
							(else midpoint))))))

(define (close-enogh? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
	(let ((a-value (f a))
				(b-value (f b)))
		(cond ((and (negative? a-value) (positive? b-value)) (search f a b))
					((and (negative? b-value) (positive? a-value)) (search f b a))
					(else (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)




