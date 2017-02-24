#lang racket

;;;tag;;;
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


;;;osaka-package;;;
; (list ('osaka name1 address1 salary1) ('osaka name2 adress2 salary2) ...)
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
	(define (get-record name files)
		(cond ((null? files) #f)
					((eq? name (get-name (car files))) (tag (car files)))
					(else (get-record name (cdr files)))))

	(put 'get-name '(osaka)
			 (lambda (record) (get-name record)))
	(put 'make-record '(osaka)
			 (lambda (name adress salary) (make-record name adress salary)))
	(put 'get-salary '(osaka)
			 (lambda (record) (get-salary record)))
	(put 'insert-record '(osaka)
			 (lambda (record file) (save-record record file)))
	;apply-genericでデータがタグ付き前提なので'(osaka osaka)	
	(put 'get-record '(() osaka)
			 (lambda (name file) (get-record name file)))
	(put 'test-data 'osaka
			 (tag (list '(name1 adress1 salary1)
									'(name2 adress2 salary2)
									'(hyoga ibaraki 10000000)
									'(hoge koko 1)
									'(foo soko 10))))
	'done)


;;;shiga-package;;;
; '(shiga '((name tomoyat1) '(salary 0)))
(define (install-shiga-package)
	(define (selector prop)
		(define (get-prop record)
			(cond 
				((eq? '() record)
				 '())
				((eq? prop (caar record))
				 (cadar record))
				(else
					(get-prop (cdr record)))))
		get-prop)
	(define get-name (selector 'name))
	(define get-salary (selector 'salary))
	(define (insert-record record file)
		(attach-tag 'shiga (cons record file)) )
	(define (get-record key file)
		(cond
			((eq? '() file)
			 #f)
			((eq? key (get-name (car file)))
			 (attach-tag 'shiga (car file)))
			(else
				(get-record key (cdr file)))))

	(put 'get-name '(shiga) get-name)
	(put 'get-salary '(shiga) get-salary)
	(put 'insert-record '(shiga) insert-record)
	(put 'get-record '(() shiga) get-record)
	(put 'test-data 'shiga
			 (attach-tag 'shiga '(((name tomoyat1) (salary 0))
														((name tomoyat2) (salary 1)))))
	'done)

;;;kyoto;;;
(define (install-kyoto-package)
	(define (get-name local-record)
		(car local-record))  
	(define (get-salary local-record)
		(cadr local-record))  
	(define (get-record name file)
		(cond ((null? file) #f)
					((eq? name (get-name (car file))) (cons 'kyoto (car file)))
					(else (get-record name (cdr file)))))
	(define (insert-record record file)
		(cons record file))
	(put 'get-name '(kyoto)
			 (lambda (record) (get-name record)))
	(put 'get-salary '(kyoto)
			 (lambda (record) (get-salary record)))
	(put 'get-record '(() kyoto)
			 (lambda (name file) (get-record name file)))
	(put 'test-data 'kyoto
			 (cons 'kyoto
						 (list '(tanaka 67 *)
									 '(suzuki 79 **)
									 '(kaneda 100 ***))))
	'done)

;;;body;;;
(define (get-name tagged-record)
	(apply-generic 'get-name tagged-record))

(define (get-salary tagged-record)
	(apply-generic 'get-salary tagged-record))

(define (get-record key tagged-file)
	(apply-generic 'get-record (attach-tag '() key) tagged-file))

(install-osaka-package)
(install-shiga-package)
(install-kyoto-package)

(define (find-employee-record name files)
	(if (null? files)
		#f
		(let ((file (car files)))
			(if (get-record name file)
				(get-record name file)
				(find-employee-record name (cdr files))))))

(define osaka-data (get 'test-data 'osaka))
(define kyoto-data (get 'test-data 'kyoto))
(define shiga-data (get 'test-data 'shiga))
(define files (list osaka-data shiga-data kyoto-data))
(find-employee-record 'suzuki files)
(find-employee-record 'hyoga files)
(find-employee-record 'tomoyat1 files)
(find-employee-record 'you files)


;;;out;;;
; 'done
; 'done
; 'done
; '(kyoto suzuki 79 **)
; '(osaka hyoga ibaraki 10000000)
; '(shiga (name tomoyat1) (salary 0))
; #f

