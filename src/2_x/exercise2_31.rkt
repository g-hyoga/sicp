#lang racket

(define (square x)
	(* x x))

#;(define (square-tree tree)
	(cond ((not (pair? tree)) (square tree))
				(else (map square-tree tree))))

#;(define (square-tree tree)
	(cond ((null? tree) null)
				((not (pair? tree)) (square tree))
				(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))
;;;;;;
(define (tree-map proc tree)
	(cond ((not (pair? tree)) (proc tree))
				(else (map (lambda (x) (tree-map proc x)) tree))))

(define (square-tree tree)
	(tree-map square tree))
;;;;;;

(square-tree
	(list 1
				(list 2 (list 3 4) 5)
				(list 6 7)))

(define (tree-map proc tree)
	(cond ((not (pair? tree)) (proc tree))
				(else (map (lamda (x) tree-map proc x) tree))))

(define (tree-map proc tree)
	(cond ((null? tree) null)
				((not (pair? tree)) (proc tree))
				(else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

