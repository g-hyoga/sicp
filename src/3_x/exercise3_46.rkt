#lang planet neil/sicp

(define (test-and-set! cell)
	(if (car cell)
		#t 
		(begin (set-car! cell #t)
					 #f)))

(define (make-mutex)
	(let ((cell (list #f)))
		(define (the-mutex m)
			(cond ((eq? m 'acquire)
						 (if (test-and-set! cell)
							 (the-mutex 'acquire)))
						((eq? m 'release) (clear! cell))))
		the-mutex))

(define (clear! cell) (set-car! cell #f))

(define (make-semaphore n)
	(let ((mutex (make-mutex)))
		(define (the-semaphore s)
			(cond ((eq? s 'wait)
						 (if (> n 0)
							 (begin (mutex 'aquire)
											(set! n (- n 1))
											(mutex 'release))
							 (the-semaphore 'wait)))
						((eq? s 'signal)
						 (begin (mutex 'release)
										(set! n (+ n 1))
										(mutex 'release)))))
		the-semaphore))


(define s (make-semaphore 2))

(s 'wait)
(s 'wait)
(s 'wait)
