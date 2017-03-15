#lang racket

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put-table table key1 key2 item)
	(if (not (hash-has-key? table key1))
		(hash-set! table key1 (make-hash))
		true)
	(hash-set! (hash-ref table key1) key2 item))

(define (get-table table key1 key2)
	(define (not-found . msg)
		;  (display msg (current-error-port))
		;  (display "\n")
		false)
	(if (hash-has-key? table key1)
		(if (hash-has-key? (hash-ref table key1) key2)
			(hash-ref (hash-ref table key1) key2)
			(not-found "Bad key -- KEY2" key2))
		(not-found "Bad key -- KEY1" key1)))

(define *op-table* (make-hash))
(define (put op type item)
	(put-table *op-table* op type item))
(define (get op type)
	(get-table *op-table* op type))

(define *coercion-table* (make-hash))
(define (put-coercion type1 type2 item)
	(put-table *coercion-table* type1 type2 item))
(define (get-coercion type1 type2)
	(get-table *coercion-table* type1 type2))

(provide put)
(provide get)
(provide put-coercion)
(provide get-coercion)
