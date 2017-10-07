#lang planet neil/sicp

(define (count-pairs x)
	(define counted '())
	(define (include? target li)
		(cond ((not (pair? li)) #f)
					((eq? target (car li)) #t)
					(else (include? target (cdr li)))))
	(define (iter li)
		(cond ((not (pair? li)) 0)
					((include? li counted) 0)
					(else (+ (begin (set! counted (append counted (list li)))
													(iter (car li)))
									 (begin (set! counted (append counted (list li)))
													(iter (cdr li)))
									 1))))
	(iter x))

(define x (list 1 2 3))
(count-pairs x)

(define y (list 1 2 3))
(set-car! y (cdr (cdr y)))
(count-pairs y)

(define z (list 1 2 3))
(set-car! z (cdr z))
(set-car! (cdr z) (cdr (cdr z)))
(count-pairs z)

