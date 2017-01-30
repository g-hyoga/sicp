#lang racket

(define (square n) (* n n))

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (next test-divisor)))))


(define (next test-divisor)
	(if (= test-divisor 2) 3 (+ test-divisor 2)))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (expmod base exp m)
	(cond ((= exp 0) 1)
				((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
				(else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (remainder (- n 1) 999999999)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
				((fermat-test n) (fast-prime? n (- times 1)))
				(else false)))

(define (report-prime elapsed-time)
	(display " *** ") (display elapsed-time))

(define (runtime) (current-milliseconds))

(define (start-prime-test n start-time)
	(if (fast-prime? n 10)
		(report-prime (- (runtime) start-time))
		#f))

(define (time-prime-test n)
	(newline) (display n) (start-prime-test n (runtime)))

(time-prime-test 10000000037)











