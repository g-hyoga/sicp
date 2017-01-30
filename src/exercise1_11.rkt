#lang racket

(define (f_r n)
	(cond ((< n 3) n)
				((>= n 3) (+ (f_r (- n 1))
										 (* 2 (f_r (- n 2)))
										 (* 3 (f_r (- n 3)))))))


(define (f_i n)
	(if (<= n 1) n (iter 2 2 1 0 n)))

(define (iter i a b c n)
	(cond ((= i n) a)
				((< i n) (iter (+ i 1) (+ a (* 2 b) (* 3 c)) a b n))))

(f_r 25)
(f_i 25)








