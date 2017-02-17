#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

#;(define (deriv exp var)
	(cond ((number? exp) 0)
				((variable? exp) (if (same-variable? exp var) 1 0))
				((sum? exp) (make-sum (deriv (addend exp) var)
															(deriv (augend exp) var)))
				((product? exp)
				 (make-sum
					 (make-product (multiplier exp)
												 (deriv (multiplicand exp) var))
					 (make-product (deriv (multiplier exp) var)
												 (multiplicand exp))))
				((exponentiation? exp)
				 (make-product
					 (exponent exp)
					 (make-exponentiation (base exp) (- (exponent exp) 1))))
				(else (error "unknown expression type: DERIV" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (deriv exp var)
	(cond ((number? exp) 0)
				((variable? exp)
				 (if (same-variable? exp var) 1 0))
				(else ((get 'deriv (operator exp))
							 (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
	(define (make-sum a1 a2)
		(cond ((=number? a1 0) a2)
					((=number? a2 0) a1)
					((and (number? a1) (number? a2))
					 (+ a1 a2))
					(else (list '+ a1 a2))))
	(define (sum? x) (and (pair? x) (eq? (car x) '+)))
	(define (addend s) (cadr s))
	(define (augend s)
		(if (null? (cdddr s))
			(caddr s)
			(cons '+ (cddr s))))
	(define (make-product m1 m2)
		(cond ((or (=number? m1 0) (=number? m2 0)) 0)
					((=number? m1 1) m2)
					((=number? m2 1) m1)
					((and (number? m1) (number? m2)) (* m1 m2))
					(else (list '* m1 m2))))
	(define (product? x) (and (pair? x) (eq? (car x) '*)))
	(define (multiplier p) (cadr p))
	(define (multiplicand p) 
		(if (null? (cdddr p))
			(caddr p)
			(cons '* (cddr p))))
	(define (make-exponentiation b e)
		(cond ((=number? e 0) 1)
					((=number? e 1) b)
					((=number? b 0) 0)
					((=number? b 1) 1)
					((and (number? b) (number? e)) (make-exponentiation b (- e 1)))
					(else (list '** b e))))
	(define (exponentiation? x)
		(and (pair? x) (eq? (car x) '**)))
	(define (base x) (cadr x))
	(define (exponent x) (caddr x))

	(put 'deriv '+ (make-sum (deriv (addend exp) var)
													 (deriv (augend exp) var)))
	(put 'deriv '* (make-sum (make-product (multiplier exp)
																				 (deriv (multiplicand exp) var))
													 (make-product (deriv (multiplier exp) var)
																				 (multiplicand exp))))
	(put 'deriv '** (make-product (exponent exp)
																(make-exponentiation (base exp)
																										 (- (exponent exp) 1)))))

(deriv '(* 2 x) 'x)











