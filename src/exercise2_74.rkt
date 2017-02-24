#lang racket

; tag
(define (attach-tag type-tag contents)
	(cons type-tag contents))
(define (type-tag datum)
	(if (pair? datum)
		(car datum)
		(error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents dataum)
	(if (pair? dataum)
		(cdr dataum)
		(error "Bad tagged dataum: CONTENTS" dataum)))

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args)) 
				(error
					"No method for these types: APPLY-GENERIC"
					(list op type-tags))))))

;;;put get;;;
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

; (list ('osaka name1 adress1 salary1) ('osaka name2 adress2 salary2) ...)
(define (install-osaka-package)
	(define (tag record) (attach-tag 'osaka record))
	(define (make-record name adress salary)
		(tag (list name adress salary)))
	(define (get-name record)
		(car record))
	(define (get-adress record)
		(cadr record))
	(define (get-salary record)
		(caddr record))
	(define (save-record record file) (cons record file))

	(put 'get-name '(osaka)
			 (lambda (record) (get-name record)))
	(put 'make-record '(osaka)
			 (lambda (name adress salary) (make-record name adress salary)))
	(put 'get-salary '(osaka)
			 (lambda (record) (get-salary record)))
	(put 'save-record '(osaka)
			 (lambda (record file) (save-record record file)))
	'done)

(define (find-employee-record name files)
	(let* ((file (car files))
				 (record (car file))
				 (division (car record)))
		(define get-name (get division 'get-name))
		(cond ((null? files) #f)
					((null? file) (find-employee-record name (car files)))
					((equal? name (get-name record)))
					(else (find-employee-record name (car file))))))

(install-osaka-package)
(define file (list '(osaka name1 adress1 salary1) 
									 '(osaka name2 adress2 salary2)
									 '(osaka hyoga shima 10000000)
									 '(osaka hoge koko 1)
									 '(osaka foo soko 10)))

(define make-record (get 'make-record '(osaka)))
(define save-record (get 'save-record '(osaka)))
(define (get-name record) (apply-generic 'get-name record))
(define (get-salary record) (apply-generic 'get-salary record))

(define (get-record name files)
	(cond ((null? files) #f)
				((eq? name (get-name (car files))) (car files))
				(else (get-record name (cdr files)))))
;(define file2 (save-record (make-record 'person 'koko 100) file))

(get-record 'hyoga file)
(get-salary (get-record 'hyoga file))


