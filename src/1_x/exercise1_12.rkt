#lang racket 

(define (pascal n m)
	(cond ((<= n m) 1)
				((= m 1) 1)
				(else (+ (pascal (- n 1) (- m 1))
								 (pascal (- n 1) m)))))


(pascal 5 3)








