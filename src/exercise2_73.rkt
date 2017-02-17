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
(define (attach-tag operator operand) (cons operator operand))

; operand を扱う
(define (install-deriv-sum-package)
	(define (make a1 a2)
		(cond ((=number? a1 0) a2)
					((=number? a2 0) a1)
					((and (number? a1) (number? a2))
					 (+ a1 a2))
					(else (attach-tag '+ (list a1 a2)))))
	(define (addend operand) (car operand))
	(define (augend operand) (cadr operand))
	(define (deriv operand var)
		(make-sum (deriv (addend operand) var)
							(deriv (augend operand) var)))
	(put 'make '+ make)
	(put 'deriv '+ deriv)
	'done)

(define (install-deriv-product-package)
	(define (make m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
				((=number? m1 1) m2)
				((=number? m2 1) m1)
				((and (number? m1) (number? m2)) (* m1 m2))
				(else (attach-tag '* (list m1 m2)))))
	(define (multiplier operand) (car operand))
	(define (multiplicand operand) (cadr operand))
	(define make-sum (get '+ 'make))
	(define (deriv operand var)
		(make-sum
					 (make-product (multiplier exp)
												 (deriv (multiplicand exp) var))
					 (make-product (deriv (multiplier exp) var)
												 (multiplicand exp))))
	(put 'make '* make)
	(put 'deriv '* deriv)
	'done)

(define (install-deriv-exponentiation-package)
	(define (make b e)
		(cond ((=number? e 0) 1)
					((=number? e 1) b)
					((=number? b 0) 0)
					((=number? b 1) 1)
					((and (number? b) (number? e)) (make-exponentiation b (- e 1)))
					(else (list '** b e))))
	(define (base x) (car x))
	(define (exponent x) (cadr x))
	(define make-product (get '* make))
	(define (deriv operand var)
		(make-product
			(exponent exp)
			(make-exponentiation (base exp) (- (exponent exp) 1))))
	(put 'deriv '** deriv)
	'done)

(deriv '(* 2 x) 'x)











