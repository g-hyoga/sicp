#lang racket

(define (make-interval a b) (cons a b))

(define (upper-bound x)
	(cond ((> (car x) (cdr x)) (car x))
				(else (cdr x))))

(define (lower-bound x)
	(cond ((< (car x) (cdr x)) (car x))
				(else (cdr x))))

(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
								 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
	(make-interval (- (lower-bound x) (lower-bound y))
								 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
	(let ((p1 (* (lower-bound x) (lower-bound y)))
				(p2 (* (lower-bound x) (upper-bound y)))
				(p3 (* (upper-bound x) (lower-bound y)))
				(p4 (* (upper-bound x) (upper-bound y))))
		(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (spans-zero? x)
	(>= 0 (* (lower-bound x) (upper-bound x))))

(define (div-interval x y)
	(cond ((spans-zero? y) (error "spans zero"))
				(else (mul-interval 
								x
								(make-interval (/ 1.0 (lower-bound y))
															 (/ 1.0 (upper-bound y)))))))


(define x (make-interval 1 2))
(define y (make-interval 3 0))

(div-interval x y)







