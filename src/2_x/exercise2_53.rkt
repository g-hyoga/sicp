#lang racket

(define (memp item x)
	(cond ((null? x) false)
				((eq? item (car x)) x)
				(else (memp item (cdr x)))))

(memp 'apple '(pear banana prune))

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

