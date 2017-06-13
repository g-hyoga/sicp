#lang planet neil/sicp

(define (make-table same-key?)
	(define (assoc key records)
		(cond ((null? records) #f)
					((same-key? key (caar records)) (car records))
					(else (assoc key (cdr records)))))
	
	(let ((local-table (list '*table*)))
		; 1dim
		(define (helper1 key p1 p2)
			(let ((record (assoc key (cdr local-table))))
				(if record 
					(p1 record)
					(p2))))
		(define (insert1! key value)
			(helper1 key
							 (lambda (record) (set-cdr! record value))
							 (lambda () (set-cdr! local-table
																		(cons (cons key value)
																					(cdr local-table)))))
			'ok)
		(define (lookup1 key)
			(helper1 key
							 (lambda (record) (cdr record))
							 (lambda () #f)))

		; 2dim
		(define (helper2 key-1 key-2 p1 p2 p3)
			(let ((subtable (assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (assoc key-2 (cdr subtable))))
						(if record 
							(p1 record)
							(p2 subtable)))
					(p3))))
		(define (lookup2 key-1 key-2)
			(helper2 key-1 key-2 cdr (lambda (x) #f) (lambda () #f)))
		(define (insert2! key-1 key-2 value)
			(helper2 key-1
							 key-2
							 (lambda (record) (set-cdr! record value))
							 (lambda (subtable) (set-cdr! subtable (cons (cons key-2 value)
																													 (cdr subtable))))
							 (lambda () (set-cdr! local-table
																		(cons (list key-1 (cons key-2 value))
																					(cdr local-table)))))
			'ok)

		(define (distinct-lookup x . y)
			(if (= (length y) 0)
				(lookup1 x)
				(lookup2 x (car y))))
		(define (distinct-insert! x . y)
			(if (= (length y) 1)
				(insert1! x (car y))
				(insert2! x (car y) (cadr y))))

		(define (dispatch m)
			(cond ((eq? m 'lookup-proc) distinct-lookup)
						((eq? m 'insert-proc!) distinct-insert!)
						(else (error "Unknown operation: TABLE" m))))
		dispatch))

(define (memoize f)
	(let ((table (make-table equal?)))
		(define lookup (table 'lookup-proc))
		(define insert! (table 'insert-proc!))
		(lambda (x)
			(let ((previously-computed-result
							(lookup x table)))
				(or previously-computed-result
						(let ((result (f x)))
							(insert! x result table)
							result))))))

(define memo-fib
	(memoize (lambda (n) 
						 (cond ((= n 0) 0)
									 ((= n 1) 1)
									 (else (+ (memo-fib (- n 1))
														(memo-fib (- n 2))))))))

(memo-fib 3)



