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


(define (make-record key value)
	(list key value))

(define (key record)
	(car record))

(define (value record)
	(cdar record))

#;(define (lookup given-key set-of-records)
	(cond ((null? set-of-records) #f)
				((equal? given-key (key (car set-of-records)))
				 (car set-of-records))
				(else (lookup given-key (cdr set-of-records)))))

(define (lookup given-key tree)
	(cond ((null? tree) #f)
				((= (key (entry tree)) given-key) (entry tree))
				((> (key (entry tree)) given-key) (lookup given-key (left-branch tree)))
				((< (key (entry tree)) given-key) (lookup given-key (right-branch tree)))
				(else #f)))

(define r1 (make-record 10 "hoge"))
(define r2 (make-record 13 "foo"))
(define r3 (make-record 16 "hyo"))
(define r4 (make-record 9 "glacier"))
(define r5 (make-record 3 "fuga"))
(define r6 (make-record 19 "hage"))

(define tree (list->tree (list r1 r2 r3 r4 r5 r6)))
(lookup 10 tree)
(lookup 13 tree)
(lookup 7 tree)














