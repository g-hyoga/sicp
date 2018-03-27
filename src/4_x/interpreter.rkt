#lang racket
(require r5rs)
(require racket/cmdline)

(define true #t)
(define false #f)
(define apply-in-underlying-scheme apply)

(define (err-handler proc seq err-message)
  (if (null? seq)
    (error err-message seq)
    (proc seq)))

(define (car-handler seq err-message)
  (err-handler car seq err-message))

(define (caadr-handler seq err-message)
  (err-handler caadr seq err-message))

(define (cadr-handler seq err-message)
  (err-handler cadr seq err-message))

(define (caddr-handler seq err-message)
  (err-handler caddr seq err-message))

(define (cadddr-handler seq err-message)
  (err-handler cadddr seq err-message))

(define (cdadr-handler seq err-message)
  (err-handler cdadr seq err-message))

(define (cdr-handler seq err-message)
  (err-handler cdr seq err-message))

(define (cddr-handler seq err-message)
  (err-handler cddr seq err-message))

(define (cdddr-handler seq err-message)
  (err-handler cdddr seq err-message))

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
  (cadr-handler exp "TEXT_Of_QUOTATION exp is null" exp))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr-handler exp "ASSGNMENT_VARIABLE exp is null"))

(define (assinment-value exp) 
  (caddr-handler exp "ASSIGNMENT_VALUE exp is null"))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr-handler exp "DEFINITION_VARIABLE exp is null")
    (caadr-handler exp "DEFINITION_VARIABLE exp is null")))

(define (definition-value exp)
  (if (symbol? (cadr-handler exp "DEFINITION_VALUE exp is null"))
    (caddr-handler exp "DEFINITION_VALUE exp is null")
    (make-lambda (cdadr-handler exp "DEFINITION_VALUE exp is null")
                 (cddr-handler exp "DEFINITION_VALUE exp is null"))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) 
  (cadr-handler exp "LAMBDA_PARAMETERS exp is null"))

(define (lambda-body exp) 
  (cddr-handler exp "LAMBDA_BODY exp is null"))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr-handler exp "IF_PREDICATE exp is null"))

(define (if-consequent exp)
  (caddr-handler exp "IF_CONSEQUENT exp is null"))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr-handler exp "IF_ALTERNATIVE exp is null")
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr-handler exp "BEGIN-ACTIONS exp is null"))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car-handler seq "FIRST-EXP seq is null"))

(define (rest-exps seq) (cdr-handler seq "REST-EXPS seq is null"))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car-handler exp "OPERATER exp is null"))

(define (operands exp) (cdr-handler exp "OPERANDS exp is null"))

(define (no-operands? ops) (null? ops))

(define (first-operand ops)
  (car-handler ops "FIRST-OPERAND ops is null"))

(define (rest-operands ops) 
  (cdr-handler ops "FIRST-OPERAND ops is null"))

;;;;; ex4.5 ;;;;;

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) 
  (cdr-handler exp "COND_CLAUSES exp is null"))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) 
  (car-handler clause "COND_PREDICATE clause is null"))

(define (cond-recipient-clause? clause)
  (if (null? (cadr clause))
    false
    (eq? (cond-recipient-symbol clause) '=>)))

(define (cond-recipient-actions clause)
  (cddr-handler clause "COND_RECIPIENT_ACTIONS clause is null"))

(define (cond-recipient-symbol clause) 
  (cadr-handler clause "COND_PREDICATE_SYMBOL clause is null"))

(define (cond-actions clause)
  (cdr-handler clause "COND_ACTIONS clause is null"))

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

(define (procedure-parameters p)
  (cadr-handler p "PROCEDURE_PARAMETERS p is null"))

(define (procedure-body p) 
  (caddr-handler p "PROCEDURE_BODY p is null"))

(define (procedure-environment p) 
  (cadddr-handler p "PROCEDURE_ENVIRONMENT p is null"))

(define (enclosing-environment env)
  (cdr-handler env "ENCLOSING_ENVIRONMENT env is null"))

(define (first-frame env)
  (car-handler env "FIRST_FRAME env is null"))

(define (rest-frames env)
  (cdr-handler env "REST_FRAMES env is null"))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car-handler frame "FRMAE_VARIABLES frame is null"))

(define (frame-values frame)
  (cdr-handler frame "FRAME_VALUES is null"))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (first-frame frame)))
  (set-cdr! frame (cons val (rest-frames frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many argumants supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

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

(define (primitive-implementation proc) 
  (cadr-handler proc "PRIMITIVE_IMPLEMENTION proc is null"))

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
  (map (lambda (x) (car-handler x "PRIMITIVE_PROCEDURE_NAME")) 
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr-handler proc "PRIMITIVE_PROCEDURE_OBJECTS")))
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

(define (and-conjuncts exp) 
  (cdr-handler exp "AND_CONJUNCTS"))

(define (or? exp) (tagged-list? exp 'or))

(define (or-conjuncts exp)
  (cdr-handler exp "OR_CONJUNCTS"))

(define (eval-and conjuncts env)
  (cond ((null? conjuncts) false)
        ((null? (cdr conjuncts)) 
         (eval (car conjuncts) env))
        ((eval (car conjuncts) env)
         (eval-and (cdr conjuncts) env))
        (else false)))

(define (eval-or disconjuncts env)
  (if (null? disconjuncts)
    false
    (let ((evaled-val (eval (car disconjuncts) env)))
      (cond (evaled-val evaled-val)
            (else (eval-or (cdr disconjuncts) env))))))

;;;;; ex4.6 ;;;;;

(define (let? exp) (eq? (car exp) 'let))

(define (let-bindings exp)
  (cadr-handler exp "LET_BINDING"))

(define (let-body exp)
  (cddr-handler exp "LET_BODY"))

;;;;; ex4.7 ;;;;;

(define (let*? exp)
  (eq? (car exp) 'let*))

(define (make-let variables body)
  (if (null? body)
    (error "let-body is null")
    (list 'let variables body)))

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

; (let <var> <binding> <body>)
; it is syntax suger
; ((lambda (<binding-variables>) 
;    (define (<var> <binding-variables>) <body>) 
;    (<var> <binding-variables>)) <binding-values>)

(define (make-definition var exp)
  (list 'define var exp))

(define (name-let? exp)
  (not (null? (name-let-body exp))))

(define (name-let-variable exp) 
  (cadr-handler exp "NAME_LET_VARIABLE"))

(define (name-let-binding exp)
  (caddr-handler exp "NAME_LET_BINDING"))

(define (name-let-binding-vars exp)
  (map (lambda (x) (car-handler x "NAME_LET_BINDING_VARS"))
       (name-let-binding exp)))

(define (name-let-binding-vals exp)
  (map (lambda (x) (cadr-handler x "NAME_LET_BINDING_VALS"))
       (name-let-binding exp)))

(define (name-let-body exp) 
  (cdddr-handler exp "NAME_LET_BODY"))

(define (let->name-let exp)
  (cons (make-lambda
          (name-let-binding-vars exp)
          (list (make-begin
                  (list (make-definition 
                          (cons (name-let-variable exp)
                                (name-let-binding-vars exp))
                          (make-begin (name-let-body exp)))
                        (cons (name-let-variable exp) 
                              (name-let-binding-vars exp))))))
        (name-let-binding-vals exp)))

(define (let->combination exp)
  (cond ((name-let? exp) (let->name-let exp))
        (else (cons (make-lambda 
                      (map car (let-bindings exp)) 
                (let-body exp))
              (map cadr (let-bindings exp))))))

;;;;; ex4.12 ;;;;;

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

;;;;; eval ;;;;;
(define the-global-environemt (setup-environment))
;(driver-loop)
(define raw-input (vector-ref (current-command-line-arguments) 0))
;(define raw-input "(define (fib n)
;  (let fib-iter ((a 1)
;                 (b 0)
;                 (count n))
;    (if (= count 0)
;      b
;      (fib-iter (+ a b) a (- count 1)))))
;(fib 5)")

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
