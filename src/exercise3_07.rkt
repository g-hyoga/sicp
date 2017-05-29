#lang racket

(define (make-account balance pass)
	(let ((passwords (list pass)))
		(define (withdraw amount)
			(if (>= balance amount)
				(begin (set! balance (- balance amount)) balance)
				"Insufficient funds"))
		(define (deposit amount)
			(set! balance (+ balance amount))
			balance)
		(define (add-password pass)
			(begin (set! passwords (cons pass passwords))
						 dispatch))
		(define (dispatch pass m)
				(cond ((null? passwords) (error "Incorrect password"))
							((eq? pass (car passwords))
							 (cond ((eq? m 'withdraw) withdraw)
										 ((eq? m 'deposit) deposit)
										 ((eq? m 'add-password) add-password)
										 (else (error "Unknown request: MAKE-ACCOUNT" m))))
							(else (begin (set! passwords (cdr passwords))
													 (dispatch pass m)))))
		dispatch))

(define (make-joint acc pass new-pass)
	((acc pass 'add-password) new-pass))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)

(define another-acc
	(make-joint acc 'secret-password 'new-secret-password))
((another-acc 'new-secret-password 'withdraw) 40)

