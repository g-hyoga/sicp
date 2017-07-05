#lang planet neil/sicp

; inverterなどで 
; 0 - inverter - 0
; となってしまうのを初期化して
; 0 - inverter - 1
; にしている

(define (logical-not s)
	(cond ((= s 0) 1)
				((= s 1) 0)
				(else (error "Invalid signal" s))))

(define (logical-and a b)
	(cond ((= a b 1) 1)
	(cond ((= a b 0) 0)
	(cond ((and (= a 0) (= b 1)) 0)
	(cond ((and (= a 1) (= b 0)) 0)
				(else (error "Invalid signal" a b))))

(define (logical-or a b)
	(cond ((= a b 1) 1)
	(cond ((= a b 0) 0)
	(cond ((and (= a 0) (= b 1)) 1)
	(cond ((and (= a 1) (= b 0)) 1)
				(else (error "Invalid signal" a b))))

(define (inverter input output)
	(define (invert-input)
		(let ((new-value (logical-not (get-signal input))))
			(after-delay inverter-delay
									 (lambda () (set-signal! output new-value)))))
	(add-action! input invert-input) 'ok)

(define (and-gate a1 a2 output)
	(define (and-action-procedure)
		(let ((new-value
						(logical-and (get-signal a1) (get-signal a2))))
			(after-delay
				and-gate-delay
				(lamda () (set-signal! output new-value)))))
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	'ok)

(define (call-each procedures)
	(if (null? procedures)
		'done
		(begin ((car procedures))
					 (call-each (cdr procedures)))))

(define (make-wire)
	(let ((signal-value 0) (action-procedures '()))
		(define (set-my-signal! new-value)
			(if (not (= signal-value new-value))
				(begin (set! signal-value new-value)
							 (call-each action-procedures))
				'done))
		(define (accept-action-procedure! proc)
			(set! action-procedures
				(cons proc action-procedures))
			(proc))
		(define (dispatch m)
			(cond ((eq? m 'get-signal) signal-value)
						((eq? m 'set-signal!) set-my-signal!)
						((eq? m 'add-action!) accept-action-procedure!)))
		dispatch))









