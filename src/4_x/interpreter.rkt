#lang racket
(require r5rs)
(require racket/cmdline)

(define true #t)
(define false #f)
(define apply-in-underlying-scheme apply)

;;;;; 4.1.1 ;;;;;

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
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((and? exp) (eval-and (and-conjuncts exp) env))
        ((or? exp) (eval-or (or-conjuncts exp) env))
        ((application? exp)
         (myapply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: eval" exp))))

(define (myapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type: myapply" procedure))))

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

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assinment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;;;; 4.1.2 ;;;;;

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assinment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;;;; ex4.5 ;;;;;

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-recipient-clause? clause)
  (if (null? (cadr clause))
    false
    (eq? (cond-recipient-symbol clause) '=>)))

(define (cond-recipient-actions clause) (cddr clause))

(define (cond-recipient-symbol clause) (cadr clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (cond ((cond-else-clause? first)
             (if (null? rest)
               (sequence->exp (cond-actions first))
               (error "ELSE clause isn't last: COND->IF" clauses)))
            ((cond-recipient-clause? first)
             (make-let (list (list 'tmp (cond-predicate first)))
                       (make-if 'tmp 
                                (sequence->exp 
                                  (map 
                                    (lambda (action) (list action 'tmp)) 
                                    (cond-recipient-actions first)))
                                (expand-clauses rest))))
            (else (make-if (cond-predicate first)
                           (sequence->exp (cond-actions first))
                           (expand-clauses rest)))))))

;;;;; 4.1.3 ;;;;;

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many argumants supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variables" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variables: SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))
  

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;;;; 4.1.4 ;;;;;

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'cons mcons)
        (list 'car mcar)
        (list 'cdr mcdr)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '% remainder)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'string? string?)
        (list 'number? number?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; M-eval input:")

(define output-prompt ";;; M-eval value")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environemt)))
      (announce-output output-prompt)
      (display output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))

;;;;; ex.4.4 ;;;;;

(define (and? exp) (tagged-list? exp 'and))

(define (and-conjuncts exp) (cdr exp))

(define (or? exp) (tagged-list? exp 'or))

(define (or-conjuncts exp) (cdr exp))

(define (eval-and conjunction env)
  (cond ((null? (cdr conjunction)) 
         (eval (car conjunction) env))
        ((eval (car conjunction) env)
         (eval-and (cdr conjunction) env))
        (else false)))

(define (eval-or conjunction env)
  (cond ((null? conjunction) false)
        ((eval (car conjunction) env) true)
        (else (eval-or (cdr conjunction) env))))

;;;;; ex4.6 ;;;;;

(define (let? exp) (eq? (car exp) 'let))

(define (let-bindings exp)
  (cadr exp))

(define (let-body exp)
  (if (null? (cddr exp))
    (error "let body is blanked: " exp)
    (cddr exp)))

;;;;; ex4.7 ;;;;;

(define (let*? exp)
  (eq? (car exp) 'let*))

(define (make-let variables body)
  (list 'let variables body))
(define (let*->nested-lets exp)
  (let ((body (let-body exp))
        (variables (let-bindings exp)))
    (define (iter vars)
      (if (null? (cdr vars))
        (make-let (list (car vars))
                  (car body))
        (make-let (list (car vars))
                  (iter (cdr vars)))))
    (iter variables)))

;;;;; ex4.8 ;;;;;

(define (let->combination exp)
  (cons (make-lambda 
          (map car (let-bindings exp)) 
          (let-body exp))
        (map cadr (let-bindings exp))))

;;;;; eval ;;;;;
(define the-global-environemt (setup-environment))
;(driver-loop)
(define raw-input (vector-ref (current-command-line-arguments) 0))
;(define raw-input "(define hoge 1) hoge")

(define input (open-input-string raw-input))

(define (eval-iter)
  (let ((in (read input)))
    (if (not (eof-object? in))
      (let ((val (eval in the-global-environemt)))
        (if (not (eq? 'ok val))
          (begin (display val)
                 (display "\n")
                 (eval-iter))
          (eval-iter)))
      (display ""))))

(eval-iter) 

