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

; (list ('osaka name1 address1 salary1) ('osaka name2 adress2 salary2) ...)
(define (install-osaka-package)
	(define (tag record) (cons 'osaka record))
	(define (make-record name address salary)
		(tag (list name address salary)))
	(define (get-name record)
		(cadr record))
	(define (get-address record)
		(caddr record))
	(define (get-salary record)
		(cadddr record))
	(define (get-record name file)
		(cond ((null? file) #f)
					((eq? name (get-name (car file))) (car file))
					(else (get-record name (cdr file)))))
	(define (save-record record file) (cons record file))

	(put 'make-record 'osaka
			 (lambda (name address salary) (make-record name adress salary)))
	(put 'get-record 'osaka 
			 (lambda (name file) (get-record name file)))
	(put 'get-salary 'osaka
			 (lambda (record) (get-salary record)))
	(put 'save-record 'osaka
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
(define file (list '(osaka name1 address1 salary1) '(osaka name2 adress2 salary2)))
(define make-record (get 'make-record 'osaka))
(define save-record (get 'save-record 'osaka))
;(define get-record (get 'get-record 'osaka))
(define (get-record name file) (apply-generic 'get-record name file))
(define get-salary (get 'get-salary 'osaka))
(define file2 (save-record (make-record 'person 'koko 100) file))

(get-record 'person file2)
(get-salary (get-record 'person file2))


