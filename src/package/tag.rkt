#lang racket
(require "get-put.rkt")

(define (attach-tag type-tag contents)
	(cons type-tag contents))
(define (type-tag datum)
	(cond ((pair? datum) (car datum))
				((number? datum) 'scheme-number)
				(else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
	(cond ((pair? datum) (cdr datum))
				((number? datum) datum)
				(else (error "Bad tagged datum: CONTENTS" datum))))

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags))) 
			(if proc
				(apply proc (map contents args))
				(if (= (length args) 2)
					(let ((type1 (car type-tags)) 
								(type2 (cadr type-tags))
								(a1 (car args))
								(a2 (cadr args)))
						(let ((t1->t2 (get-coercion type1 type2))
									(t2->t1 (get-coercion type2 type1))) 
							(cond  ((eq? type1 type2)
											(if (t1->t2 a1 a2)
												(t1->t2 a1 a2)
												(error "No method for these types"
															 (list op type-tags))))
										 (t1->t2 
											 (apply-generic op (t1->t2 a1) a2))
										 (t2->t1
											 (apply-generic op a1 (t2->t1 a2))) 
										 (else (error "No method for these types"
																	(list op type-tags))))))
					(error "No method for these types" (list op type-tags)))))))

(provide attach-tag)
(provide type-tag)
(provide contents)
(provide apply-generic)
