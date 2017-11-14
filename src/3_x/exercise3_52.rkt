#lang racket

;-----------------------------------------------------------------

(define strem-null? null?)

(define the-empty-stream '())

(define (cons-stream a b)
	(cons a (delay b)))

#;(define (delay delayed-object)
	(memo-proc delayed-object))

(define (delay exp)
	(lambda () (exp)))

(define (force delayed-object) (delayed-object))

(define (stream-car x) (car x))
(define (stream-cdr x) (force (cdr x)))

(define (memo-proc proc)
	(let ((already-run? #f) (result #f))
		(lambda ()
			(if (not already-run?)
				(begin (set! result (proc))
							 (set! already-run? true)
							 result)
				result))))
(define (stream-map proc . argstreams)
	(if (strem-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map
						 (cons proc (map stream-cdr argstreams))))))

(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream
			low
			(stream-enumerate-interval (+ low 1) high))))

(define (display-stream x) (display (stream->list x)))

;-----------------------------------------------------------------

(define x (cons-stream 1 2))

(stream-car x)
(stream-cdr x)
