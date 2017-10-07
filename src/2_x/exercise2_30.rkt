#lang racket

(define (scale-list items factor)
	(if (null? items)
		null
		(cons (* (car items) factor)
					(scale-list (cdr items)
											factor))))

#;(define (map proc items)
	(if (null? items)
		null
		(cons (proc (car items))
					(map proc (cdr items)))))

(define (square-list items)
	(map (lambda (x) (* x x)) items))


;;;;;;
(define (square x)
	(* x x))

(define (square-tree tree)
	(cond ((not (pair? tree)) (square tree))
				(else (map square-tree tree))))

#;(define (square-tree tree)
	(cond ((null? tree) null)
				((not (pair? tree)) (square tree))
				(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))
;;;;;;

(square-tree
	(list 1
				(list 2 (list 3 4) 5)
				(list 6 7)))
