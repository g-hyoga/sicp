#lang racket

(define (last-pair li)
	(define (iter a)
		(if (null? (cdr a))
			(car a)
			(iter (cdr a))))
	(iter li))

(define (cc amount coin-values)
	(cond ((= amount 0) 1)
				((or (< amount 0) (no-more? coin-values)) 0)
				(else (+ (cc amount (expect-first-denomination coin-values))
								 (cc (- amount (first-denomination coin-values)) coin-values)))))

(define (expect-first-denomination items)
	(cdr items))

(define (first-denomination items)
	(car items))

(define (no-more? items)
	(null? items))

(define us-coins (list 50 25 10 5 1))

(cc 100 us-coins)


