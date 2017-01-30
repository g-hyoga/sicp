#lang racket

(define (make-point x y)
	(cons x y))

(define (x-point point)
	(car point))

(define (y-point point)
	(cdr point))

(define (make-segment p1 p2)
	(cons p1 p2))

(define (start-segment line)
	(car line))

(define (end-segment line)
	(cdr line))

(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")"))

(define (midpoint-segment line)
	(make-point (/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2)
							(/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2)))


(define line1 (make-segment (make-point -2 -2) (make-point 2 2)))
(define line2 (make-segment (make-point 2 2) (make-point -2 -2)))

(print-point (midpoint-segment line1))












