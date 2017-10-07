#lang racket

(define (square n) (* n n))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (runtime) (current-milliseconds))

(define (report-prime elapsed-time)
	(display " *** ") (display elapsed-time))

(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime (- (runtime) start-time))
		#f))

(define (time-prime-test n)
	(newline) (display n) (start-prime-test n (runtime)))

(define (search-for-primes start end num start-time)
	(cond ((= num 0) (newline) (display "end"))
				((< (- end start) 3) (newline) (display "end"))
				((prime? start) (newline) (display start) (report-prime (- (runtime) start-time)) (search-for-primes (+ start 2) end (- num 1) start-time))
				(else (search-for-primes (+ start 2) end num start-time))))

(search-for-primes 100000000000001 9999999999999999999999999999999 1 (runtime))






