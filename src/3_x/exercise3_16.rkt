#lang racket
(require r5rs)

(define (count-pairs x)
	(if (not (pair? x))
		0
		(+ (count-pairs (car x))
			 (count-pairs (cdr x))
			 1)))

(define x (list 1 2 3))
(count-pairs x)

(define y (list 1 2 3))
(set-car! y (cdr (cdr y)))
(count-pairs y)

(define z (list 1 2 3))
(set-car! z (cdr z))
(set-car! (cdr z) (cdr (cdr z)))
(count-pairs z)

