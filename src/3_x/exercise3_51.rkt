#lang planet neil/sicp

(define (strem-null? streams)
	(= streams '()))

(define (cons-stream a b)
	(cons a (delay b)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream
			low
			(stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
	(cond ((stream-null? stream) the-empty-stream)
				((pred (stream-car stream))
				 (cons-stream (stream-car stream)
											(stream-filter
												pred
												(stream-cdr stream))))
				(else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin (proc (stream-car s))
					 (stream-for-each proc (stream-cdr s)))))

(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
	(if (strem-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map car argstreams))
			(apply stream-map
						 (cons proc (map cdr argstreams))))))

(define (display-line x)
	(display x)
	(display "\n"))

(define (show x)
	(display-line x)
	x)


(define x
	(stream-map show
							(stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

