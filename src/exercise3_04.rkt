#lang racket

(define (make-account balance pass)
  (let ((incorrect-count 0))
    
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (correct)
      (set! incorrect-count 0))
    (define (call-the-cops)
             (display "calling 110\n"))
    
    (define (dispatch p m)
      (cond ((> incorrect-count 5) (lambda (x) (call-the-cops)))
            ((eq? p pass)
             (cond ((eq? m 'withdraw) (begin (correct) withdraw))
                   ((eq? m 'deposit) (begin (correct) deposit))
                   (else (error "Unknown request: MAKE-ACCOUNT" m))))
            (else (begin (set! incorrect-count (+ incorrect-count 1))
                         (lambda (x) (display "Incorrect password\n"))))))
    dispatch))
  
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)



