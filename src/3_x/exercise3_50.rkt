#lang racket

(require racket/stream)

(define strem-null? stream-empty?)

(define the-empty-stream (stream '()))

(define cons-stream stream-cons)

(define (memo-proc proc)
	(let ((already-run? #f) (result #f))
		(lambda ()
			(if (not already-run?)
				(begin (set! result (proc))
							 (set! already-run? true)
							 result)
				result))))

(define (delay delayed-object)
	(memo-proc delayed-object))

(define (stream-map proc . argstreams)
	(if (strem-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map car argstreams))
			(apply stream-map
						 (cons proc (map cdr argstreams))))))


