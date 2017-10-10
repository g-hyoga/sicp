#lang racket

(require racket/stream)

;-------
(define (square x) (* x x))

(define (prime? n)
	(= (smallest-divisor n) n))

(define (smallest-divisor n) (find-divisor n 2))

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))))
;-------

(define (sum-primes a b)
	(define (iter count accum)
		(cond ((> count b) accum)
					((prime? count)
					 (iter (+ count 1) (+ count accum)))
					(else (iter (+ count 1) accum))))
	(iter a 0))

(+ 1 2 3 5 7)
(sum-primes 0 10)

(stream-ref (stream 1 2 3) 0)

(define s (stream-cons 1 (stream 2)))

(define (cons-stream a b)
	(stream-cons a (stream b)))

(stream-ref s 0)


