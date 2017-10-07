#lang racket

;;;;;;;;;get put;;;;;;;;;;
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
	(define (put-helper k array)
		(cond ((null? array) (list(make-entry k item)))
					((equal? (key (car array)) k) array)
					(else (cons (car array) (put-helper k (cdr array))))))
	(set! global-array (put-helper (list op type) global-array)))

(define (get op type)
	(define (get-helper k array)
		(cond ((null? array) #f)
					((equal? (key (car array)) k) (value (car array)))
					(else (get-helper k (cdr array)))))
	(get-helper (list op type) global-array))

;;;;;;;;;;;tag;;;;;;;;;;;;
#;(define (attach-tag type-tag contents)
	(cons type-tag contents))
(define (type-tag datum)
	(if (pair? datum)
		(car datum)
		(error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents dataum)
	(if (pair? dataum)
		(cdr dataum)
		(error "Bad tagged dataum: CONTENTS" dataum)))

;;;;;;;;;;;generic;;;;;;;;;;;;
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(error "No method for these types: APPLY-GENERIC" (list op type-tags))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
		(make (deriv (addend operand) var)
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
	(define make-sum (get 'make '+))
	(define (deriv operand var)
		(make-sum
					 (make (multiplier exp)
												 (deriv (multiplicand exp) var))
					 (make (deriv (multiplier exp) var)
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
					((and (number? b) (number? e)) (make b (- e 1)))
					(else (list '** b e))))
	(define (base x) (car x))
	(define (exponent x) (cadr x))
	(define make-product (get 'make '*))
	(define (deriv operand var)
		(make-product
			(exponent exp)
			(make (base exp) (- (exponent exp) 1))))
	(put 'deriv '** deriv)
	'done)


(install-deriv-sum-package)
(install-deriv-product-package)
(define (deriv exp var)
	(cond ((number? exp) 0)
				((variable? exp)
				 (if (same-variable? exp var) 1 0))
				(else ((get 'deriv (operator exp))
							 (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (attach-tag operator operand) (cons operator operand))

(deriv '(* 2 x) 'x)











