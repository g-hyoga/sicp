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

;;;;;;;;;;;;;;;
(define (neg-interval x)
	(make-interval (- (upper-bound x)) (- (lower-bound x))))

(define (mul-interval x y)
	(let ((lx (lower-bound x)) (ux (upper-bound x)) (ly (lower-bound y)) (uy (upper-bound y)))
		(cond ((< ux 0) (neg-interval (mul-interval (neg-interval x) y)))
					((< uy 0) (neg-interval (mul-interval x (neg-interval y))))
					((and (> lx 0) (spans-zero? y))
					 (make-interval (* ux ly) (* ux uy)))
					((and (> lx 0) (> ly 0))
					 (make-interval (* lx ly) (* ux uy)))
					((and (> ly 0) (spans-zero? x)) 
					 (make-interval (* lx uy) (* ux uy)))
					((and (spans-zero? x) (spans-zero? y))
					 (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy)))))))	
;;;;;;;;;;;;;

(define (div-interval x y)
	(mul-interval 
		x
		(make-interval (/ 1.0 (lower-bound y))
									 (/ 1.0 (upper-bound y)))))

(define (spans-zero? x)
	(>= 0 (* (lower-bound x) (upper-bound x))))


(define x (make-interval -10 -2))
(define y (make-interval -3 4))

(mul-interval x y)







