#lang racket

(define (square x) (* x x))

(define (fib n)
	(fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
	(cond ((= count 0) b)
				((even? count)
				 (fib-iter a b (+ (square p) (square q)) (+ (* 2 p q) (square q)) (/ count 2)))
				(else (fib-iter (+ (* b q) (* a q) (* a p))
												(+ (* b p) (* a q))
												p
												q
												(- count 1)))))

#;(define (sum-odd-squares tree)
	(cond ((null? tree) 0)
				((not (pair? tree))
				 (if (odd? tree) (square tree) 0))
				(else (+ (sum-odd-squares (car tree))
								 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
	(define (next k)
		(if (> k n)
			null
			(let ((f (fib k)))
				(if (even? f)
					(cons f (next (+ k 1)))
					(next (+ k 1))))))
	(next 0))

(define (filter predicate sequence)
	(cond ((null? sequence) null)
				((predicate (car sequence))
				 (cons (car sequence)
							 (filter predicate (cdr sequence))))
				(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
				(accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
	(if (> low high)
		null
		(cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
	(cond ((null? tree) null)
				((not (pair? tree)) (list tree))
				(else (append (enumerate-tree (car tree))
											(enumerate-tree (cdr tree))))))

(define (sum-odd-sequence tree)
	(accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append seq1 seq2)
	(accumulate cons seq2 seq1))

(define (length sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map square (list 1 2 3))
(append (list 1 2) (list 3 4))
(length (list 1 2 3))







