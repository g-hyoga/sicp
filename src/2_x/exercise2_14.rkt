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
	(if (and (> (upper-bound x) 0)
					 (> (upper-bound y) 0)
					 (< (lower-bound x) 0)
					 (< (lower-bound y)))
		(let ((p1 (* (lower-bound x) (lower-bound y)))
					(p2 (* (lower-bound x) (upper-bound y)))
					(p3 (* (upper-bound x) (lower-bound y)))
					(p4 (* (upper-bound x) (upper-bound y))))
			(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))
		(make-interval (* (lower-bound x) (lower-bound y))
									 (* (upper-bound x) (upper-bound y)))))	

(define (div-interval x y)
	(mul-interval 
		x
		(make-interval (/ 1.0 (lower-bound y))
									 (/ 1.0 (upper-bound y)))))

(define (make-center-width c w)
	(make-interval (- c w) (+ c w)))

(define (center i)
	(/ (+ (lower-bound i) (upper-bound i)) 2))

;(define (width i)
;	(/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c w)
	(make-interval (- c (* c (/ w 100))) (+ c (* c (/ w 100)))))

(define (width i)
	(* (- (upper-bound i) (lower-bound i))
		 (/ 100 (* 2 (center i)))))

;;;;;;;;;;;;;;;;;;
(define (par1 r1 r2)
	(div-interval (mul-interval r1 r2)
								(add-interval r1 r2)))

(define (par2 r1 r2)
	(let ((one (make-interval 1 1)))
		(div-interval one
									(add-interval (div-interval one r1)
																(div-interval one r2)))))
;;;;;;;;;;;;;;;;;;

(define x (make-interval 1 2))
(define y (make-interval 3 4))

(par1 x y)
(par2 x y)






