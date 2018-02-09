#lang racket 

(define true #t)
(define flase #f)

; exp: 
; env:
; return:
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression tyle: EVAL" exp))))

; exp: A
; return: Boolean 
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; exp: A
; return: Boolean
(define (variable? exp) (symbol? exp))

; exp: A
; return: Boolean
(define (quoted? exp) (tagged-list? exp 'quote))

; exp: A
; tag: Quote
; return: Boolean
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

; exp: Quote
; return: ???
(define (text-of-quotation exp)
  (cadr exp))

; exp: A
; return: Boolean
(define (assignment? exp) (tagged-list? exp 'set!))

; exp: Assignment 
; return: 
(define (assignment-variable exp) (cadr exp))

; exp: Assignment
; return:
(define (assinment-value exp) (caddr exp))

; exp: A
; return: Boolean
(define (definition? exp) (tagged-list? exp 'define))

; exp: Definition
; return: Definition.variable
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

; exp: Definition
; return: Definition.value
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

; exp: A
; return: Boolean
(define (lambda? exp)
  (tagged-list? exp 'lambda))

; exp: Lambda
; return: Lambda.parameters
(define (lambda-parameters exp) (cadr exp))

; exp: Lambda
; return: Lambda.body
(define (lambda-body exp) (cddr exp))

; parameters: Lambda.parameters
; body: Lambda.body
; return: Lambda
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; exp: A
; return: Boolean
(define (if? exp) (tagged-list? exp 'if))

; exp: If
; return: If.predicate
(define (if-predicate exp) (cadr exp))

; exp: If
; return: If.consequent
(define (if-consequent exp) (caddr exp))

; exp: If
; return: If.alternative
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

; predicate
; consequent
; alternative
; return: If
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; exp: A
; return Boolean
(define (begin? exp) (tagged-list? exp 'begin))

; exp: Begin
; return: Begin.actions
(define (begin-actions exp) (cdr exp))

; seq: List[A]
; return: Boolean
(define (last-exp? seq) (null? (cdr seq)))

; seq: List[A]
; return: A
(define (first-exp seq) (car seq))

; seq: List[A]
; return: List[A]
(define (rest-exps seq) (cdr seq))

; seq: List[A]
; return: Begin
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

; seq: List[A]
; return: Begin
(define (make-begin seq) (cons 'begin seq))

; exp: A
; return: Boolean
(define (application? exp) (pair? exp))

; exp: A
; return: 
(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; exp: A
; return: Boolean
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clause)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
           (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

; procedure:
; arguments: List[]
; return:
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type: APPLY" procedure))))

; exps:
; env:
; return: List[]
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

; return: Quote
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assinment-value exp) env)
                       env)
  'ok)

; return: Quote
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

