#lang racket

#;(define (make-mobile left right)
	(list left right))

(define (make-mobile left right)
	(cons left right))

#;(define (make-branch length structure)
	(list length structure))

(define (make-branch length structure)
	(cons length structure))

;;;;;;;;;;;

(define (left-branch mobile)
	(car mobile))

#;(define (right-branch mobile)
	(car (cdr mobile)))

(define (right-branch mobile)
	(cdr mobile))

(define (branch-length branch)
	(car branch))

#;(define (branch-structure branch)
	(car (cdr branch)))

(define (branch-structure branch)
	(cdr branch))

(define (weight branch)
	(cond ((not (pair? (branch-structure branch))) (branch-structure branch))
				(else (total-weight (branch-structure branch)))))

(define (total-weight mobile)
	(+ (weight (left-branch mobile))
		 (weight (right-branch mobile))))

(define (balance-branch? branch)
	(cond ((not (pair? (branch-structure branch))) #t)
              (else (balance? (branch-structure branch)))))

(define (balance? mobile)
	(and (balance-branch? (left-branch  mobile))
             (balance-branch? (right-branch mobile))
             (= (* (branch-length (left-branch  mobile)) (weight (left-branch mobile)))
                (* (branch-length (right-branch mobile)) (weight (right-branch mobile))))))
              


;;;;;;;;;;

(define test-mobile
  (make-mobile
   (make-branch 2
                (make-mobile
                 (make-branch 2
                              (make-mobile
                               (make-branch 1 5)
                               (make-branch 1 5)))
                 (make-branch 1
                              (make-mobile
                               (make-branch 1 15)
                               (make-branch 3 5)))))
   (make-branch 3
                (make-mobile
                 (make-branch 1 10)
                 (make-branch 1 10)))))

#;(define test-mobile2
	(make-mobile
		(make-branch 2 
								 (make-mobile 
									 (make-branch 1 6)
									 (make-branch 2 3)))
		(make-branch 2 9)))

#;(total-weight test-mobile)
(balance? test-mobile)
