#lang racket
(require "get-put.rkt")

(define (attach-tag type-tag contents)
	(cons type-tag contents))
(define (type-tag datum)
	(cond ((pair? datum) (car datum))
				((number? datum) 'real)
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
				(error "No method for these types: APPLY-GENERIC" (list op type-tags))))))

(provide attach-tag)
(provide type-tag)
(provide contents)
(provide apply-generic)
