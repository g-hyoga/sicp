#lang racket

(define (make-leaf symbol weight)
	(list 'leaf symbol weight))

(define (leaf? object)
	(eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
	(list left
				right
				(append (symbols left)
								(symbols right))
				(+ (weight left)
					 (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)))
(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)))

(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
			'()
			(let ((next-branch
							(choose-branch
								(car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch)
								(decode-1 (cdr bits) tree))
					(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))

(define (choose-branch bit branch)
	(cond ((= bit 0) (left-branch branch))
				((= bit 1) (right-branch branch))
				(else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
	(cond ((null? set) (list x))
				((< (weight x) (weight (car set)))
				 (cons x set))
				(else (cons (car set) 
										(adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((pair (car pairs)))
			(adjoin-set (make-leaf (car pair)
														 (cadr pair))
									(make-leaf-set (cdr pairs))))))

(define (encode message tree)
	(if (null? message)
		'()
		(append (encode-symbol (car message) tree)
						(encode (cdr message) tree))))

(define (encode-symbol char tree)
	(cond ((not (leaf? (left-branch tree))) (cons 0 (encode-symbol char (left-branch tree))))
				((eq? (symbol-leaf (left-branch tree)) char) (list 0))
				((not (leaf? (right-branch tree))) (cons 1 (encode-symbol char (right-branch tree))))
				((eq? (symbol-leaf (right-branch tree)) char) (list 1))
				(else (error "bad character" char))))

(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

#;(define (successive-merge leaves)
	(if (= (length leaves) 1)
		(car leaves)
		(let ((s1 (symbol-leaf (car leaves)))
					(s2 (symbol-leaf (cadr leaves)))
					(w1 (weight-leaf (car leaves)))
					(w2 (weight-leaf (cadr leaves))))
			(successive-merge (adjoin-set (make-leaf (cons s1 s2) (+ w1 w2))
																		(cddr leaves))))))

(define (successive-merge leaves)
	(if (= (length leaves) 1)
		(car leaves)
		(successive-merge (adjoin-set (make-code-tree (car leaves)
																									(cadr leaves))
																	(cddr leaves)))))

(define sample-tree 
	(make-code-tree (make-leaf 'A 4)
									(make-code-tree
										(make-leaf 'B 2)
										(make-code-tree (make-leaf 'D 1)
																		(make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define A (list 'A 5))
(define B (list 'B 10))
(define C (list 'C 7))

(generate-huffman-tree (list A B C))


