#lang racket
(require "./interpreter.rkt")

; a
"
(define x 3)
-> (myapply (eval 'define <env>) ...)
-> (myapply (myapply 'define <env>) ...)
-> (myapply (myapply (myapply 'define <env>)) ...)
"

; b
(define (eval exp env)
  (cond ((application? exp)
         (myapply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
          (error "Unknown expression tyle: eval" exp))))

(define (application? exp) 
  (tagged-list? exp 'call))



