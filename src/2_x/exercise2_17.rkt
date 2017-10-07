#lang racket

(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))

(define (length items)
	(if (null? items)
		0
		(+ 1 (length (cdr items)))))

(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))))

(define (last-pair li)
	(define (iter a)
		(if (eq? null (cdr a))
			(car a)
			(iter (cdr a))))
	(iter li))

(define test (list 23 72 149 34))
(define test2 (list 1 3 5 7 9))

(last-pair test)


