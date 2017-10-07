#lang racket

;;;get put;;;
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

;;;generic;;;
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(error "No method for these types: APPLY-GENERIC" (list op type-tags))))))

;;;content;;;
(define (install-scheme-number-package)
	(define (tag x) (attach-tag 'scheme-number x))
	(put 'add '(scheme-number scheme-number)
			 (lambda (x y) (tag (+ x y))))
	(put 'sub '(scheme-number scheme-number)
			 (lambda (x y) (tag (- x y))))
	(put 'mul '(scheme-number scheme-number)
			 (lambda (x y) (tag (* x y))))
	(put 'div '(scheme-number scheme-number)
			 (lambda (x y) (tag (/ x y))))
	(put 'make 'scheme-number (lambda (x) (tag x)))
	'done)
	
(define (install-rational-package)
	(define (number x) (car x))
	(define (denom x) (cdr x))
	(define (make-rat n d)
		(let ((g (gcd n d)))
			(cons (/ n g) (/ d g))))
	(define (add-rat x y)
		(make-rat (+ (* number x) (denom y)
								 (* (number y) (denom x)))
							(* (denom x) (denom y))))
	(define (sub-rat x y)
		(make-rat (- (* (number x) (denom y))
								 (* (number y) (denom x)))
							(* (denom x) (denom y))))
	(define (mul-rat x y)
		(make-rat (* (number x) (number y))
							(* (denom x) (denom y))))
	(define (div-rat x y)
		(make-rat (* (number x) (denom y))
							(* (denom x) (number y))))

	(define (tag x) (attach-tag 'rational x))
	(put 'add '(rational rational)
			 (lambda (x y) (tag (add-rat x y))))
	(put 'sub '(rational rational)
			 (lambda (x y) (tag (sub-rat x y))))
	(put 'mul '(rational rational)
			 (lambda (x y) (tag (mul-rat x y))))
	(put 'div '(rational rational)
			 (lambda (x y) (tag (div-rat x y))))
	(put 'make 'rational
			 (lambda (n d) (tag (make-rat n d))))
	'done)

(define (install-rectangular-package)
	; internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (square x) (* x x))
	(define (magnitude z)
		(sqrt (+ (square (real-part z))
						 (square (imag-part z)))))
	(define (angle z)
		(atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
		(cons (* r (cos a)) (* r (sin a))))

	; interface to the rest of the system
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part) 
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag 'rectangular
			 (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular
			 (lambda (r a) (tag (make-from-mag-ang r a))))
	'done)

(define (install-polar-package)
	;; internal procedures
	(define (magnitude z) (car z))
	(define (angle z) (cdr z))
	(define (make-from-mag-ang r a) (cons r a)) 
	(define (real-part z)
		(* (magnitude z) (cos (angle z)))) 
	(define (imag-part z)
		(* (magnitude z) (sin (angle z)))) 
	(define (square x) (* x x))
	(define (make-from-real-imag x y)
		(cons (sqrt (+ (square x) (square y)))
					(atan y x)))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'polar x))
	(put 'real-part '(polar) real-part)
	(put 'imag-part '(polar) imag-part)
	(put 'magnitude '(polar) magnitude)
	(put 'angle '(polar) angle)
	(put 'make-from-real-imag 'polar
			 (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'polar
			 (lambda (r a) (tag (make-from-mag-ang r a))))
	'done)

(define (install-complex-package)
	(install-polar-package)
	(install-rectangular-package)
	(define (make-from-real-imag x y)
		((get 'make-from-real-imag 'rectangular) x y))	
	(define (make-from-mag-ang r a)
		((get 'make-from-mag-ang 'polar) r a))
	(define (real-part z) (apply-generic 'real-part z))
	(define (imag-part z) (apply-generic 'imag-part z))
	(define (magnitude z) (apply-generic 'magnitude z))
	(define (angle z) (apply-generic 'angle z))

	(define (add-complex z1 z2)
		(make-from-real-imag (+ (real-part z1) (real-part z2))
												 (+ (imag-part z1) (imag-part z2))))
	(define (sub-complex z1 z2)
		(make-from-real-imag (- (real-part z1) (real-part z2))
												 (- (imag-part z1) (imag-part z2))))
	(define (mul-complex z1 z2)
		(make-from-real-imag (* (magnitude z1) (magnitude z2))
												 (+ (angle z1) (angle z2))))
	(define (div-complex z1 z2)
		(make-from-real-imag (/ (magnitude z1) (magnitude z2))
												 (- (angle z1) (angle z2))))
	
	(define (tag z) (attach-tag 'complex z))
	(put 'add '(complex complex)
			 (lambda (z1 z2) (tag (add-complex z1 z2))))
	(put 'sub '(complex complex)
			 (lambda (z1 z2) (tag (sub-complex z1 z2))))
	(put 'mul '(complex complex)
			 (lambda (z1 z2) (tag (mul-complex z1 z2))))
	(put 'div '(complex complex)
			 (lambda (z1 z2) (tag (div-complex z1 z2))))
	(put 'make-from-real-imag 'complex
			 (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'complex
			 (lambda (r a) (tag (make-from-mag-ang r a))))
	(put 'real-part '(complex) real-part)
	(put 'imag-part '(complex) imag-part)
	(put 'magnitude '(complex) magnitude)
	(put 'angle '(complex) angle)
	'done)


;;;body;;;
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define (make-scheme-number n)
	((get 'make 'scheme-number) n))
(define (make-reational n d)
	((get 'make 'rational n d)))
(define (make-complex-from-real-imag x y)
	((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
	((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define z1 (make-complex-from-mag-ang 3 4))
(define z2 (make-complex-from-real-imag 3 4))

(define (magnitude z) (apply-generic 'magnitude z))
(magnitude z1)
(magnitude z2)





