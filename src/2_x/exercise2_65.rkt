#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
	(cond ((null? set) #f)
				((= x (entry set)) #t)
				((< x (entry set)) (element-of-set? x (left-branch set)))
				((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
	(define branced-tree
		(list->tree (tree->list set)))
	(cond ((null? branced-tree) (make-tree x '() '()))
				((= x (entry branced-tree)) branced-tree)
				((< x (entry branced-tree)) 
				 (make-tree (entry branced-tree)
										(adjoin-set x (left-branch branced-tree))
										(right-branch branced-tree)))
				((> x (entry branced-tree))
				 (make-tree (entry branced-tree)
										(left-branch branced-tree)
										(adjoin-set x (right-branch branced-tree))))))

(define (tree->list tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list (left-branch tree)
										(cons (entry tree)
													(copy-to-list
														(right-branch tree)
														result-list)))))
	(copy-to-list tree '()))

(define (list->tree elements)
	(car (partical-tree elements (length elements))))
(define (partical-tree elts n)
	(if (= n 0)
		(cons '() elts)
		(let ((left-size (quotient (- n 1) 2)))
			(let ((left-result (partical-tree elts left-size)))
				(let ((left-tree (car left-result))
							(non-left-elts (cdr left-result))
							(right-size (- n (+ left-size 1))))
					(let ((this-entry (car non-left-elts))
								(right-result (partical-tree (cdr non-left-elts) right-size)))
						(let ((right-tree (car right-result))
									(remaining-elts
										(cdr right-result)))
							(cons (make-tree this-entry
															 left-tree
															 right-tree)
										remaining-elts))))))))

(define (union set1 set2)
	(cond ((null? set1) set2)
				((null? set2) set1)
				(else (let ((x1 (car set1)) (x2 (car set2)))
								(cond 
									((= x1 x2) (cons x1 (union (cdr set1) (cdr set2))))
									((< x1 x2) (cons x1 (union (cdr set1) set2)))
									((> x1 x2) (cons x2 (union set1 (cdr set2)))))))))

(define (intersection set1 set2)
	(if (or (null? set1) (null? set2))
		'()
		(let ((x1 (car set1)) (x2 (car set2)))
			(cond ((= x1 x2) (cons x1 (intersection (cdr set1) (cdr set2))))
						((< x1 x2) (intersection (cdr set1) set2))
						((> x1 x2) (intersection set1 (cdr set2)))))))

(define (union-set set1 set2)
	(list->tree (union (tree->list set1) (tree->list set2))))
(define (intersection-set set1 set2)
	(list->tree (intersection (tree->list set1) (tree->list set2))))


(define tree1 
	(make-tree 10
						 (make-tree 5
												'()
												'())
						 (make-tree 15
												(make-tree 11
																	 '()
																	 '())
												(make-tree 19
																	 '()
																	 '()))))

(define tree2 (make-tree 7 
												 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())) 
												 (make-tree 9 '() (make-tree 11 '() '()))))


(union-set tree1 tree2)
(intersection-set tree1 tree2)
