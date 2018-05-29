#lang racket
(require r5rs)
(require racket/cmdline)

(define true #t)
(define false #f)
(define apply-in-underlying-scheme apply)

(define (err-handler proc seq err-message)
  (cond ((null? proc) (error err-message proc))
        ((null? seq) (error err-message seq))
        (else (proc seq))))

(define (stack-trace command . args)
  (define (iter args)
    (if (not (null? args))
      (begin (display " ")
             (display (car args))
             (iter (cdr args)))))
  (if (equal? (getenv "INTERPRETER_DEBUG_MODE") "true")
    (begin (display "[")
           (display command)
           (display "] ")
           (iter args)
           (display "\n"))))

(define (car-handler seq err-message) (err-handler car seq err-message))

(define (caar-handler seq err-message)
  (err-handler caar seq err-message))

(define (caadr-handler seq err-message)
  (err-handler caadr seq err-message))

(define (cadr-handler seq err-message)
  (err-handler cadr seq err-message))

(define (caddr-handler seq err-message)
  (err-handler caddr seq err-message))

(define (cadddr-handler seq err-message)
  (err-handler cadddr seq err-message))

(define (cdar-handler seq err-message)
  (err-handler cdar seq err-message))

(define (cdadr-handler seq err-message)
  (err-handler cdadr seq err-message))

(define (cdr-handler seq err-message)
  (err-handler cdr seq err-message))

(define (cddr-handler seq err-message)
  (err-handler cddr seq err-message))

(define (cdddr-handler seq err-message)
  (err-handler cdddr seq err-message))

(define (set-car!-handler seq e err-message)
  (if (null? seq)
    (error err-message)
    (set-car! seq e)))

;;;;; 4.1.1 ;;;;;

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

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
  (cadr-handler exp "TEXT_Of_QUOTATION" exp))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr-handler exp "ASSGNMENT_VARIABLE"))

(define (assinment-value exp) 
  (caddr-handler exp "ASSIGNMENT_VALUE"))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr-handler exp "DEFINITION_VARIABLE")
    (caadr-handler exp "DEFINITION_VARIABLE")))

(define (definition-value exp)
  (if (symbol? (cadr-handler exp "DEFINITION_VALUE"))
    (caddr-handler exp "DEFINITION_VALUE")
    (make-lambda (cdadr-handler exp "DEFINITION_VALUE")
                 (cddr-handler exp "DEFINITION_VALUE"))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) 
  (cadr-handler exp "LAMBDA_PARAMETERS"))

(define (lambda-body exp) 
  (cddr-handler exp "LAMBDA_BODY"))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr-handler exp "IF_PREDICATE"))

(define (if-consequent exp)
  (caddr-handler exp "IF_CONSEQUENT"))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr-handler exp "IF_ALTERNATIVE")
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr-handler exp "BEGIN-ACTIONS"))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car-handler seq "FIRST-EXP"))

(define (rest-exps seq) (cdr-handler seq "REST-EXPS"))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car-handler exp "OPERATER"))

(define (operands exp) (cdr-handler exp "OPERANDS"))

(define (no-operands? ops) (null? ops))

(define (first-operand ops)
  (car-handler ops "FIRST-OPERAND"))

(define (rest-operands ops) 
  (cdr-handler ops "FIRST-OPERAND"))

;;;;; ex4.5 ;;;;;

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) 
  (cdr-handler exp "COND_CLAUSES"))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) 
  (car-handler clause "COND_PREDICATE"))

(define (cond-recipient-clause? clause)
  (if (null? (cadr clause))
    false
    (eq? (cond-recipient-symbol clause) '=>)))

(define (cond-recipient-actions clause)
  (cddr-handler clause "COND_RECIPIENT_ACTIONS"))

(define (cond-recipient-symbol clause) 
  (cadr-handler clause "COND_PREDICATE_SYMBOL"))

(define (cond-actions clause)
  (cdr-handler clause "COND_ACTIONS"))

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
  (cadr-handler p "PROCEDURE_PARAMETERS"))

(define (procedure-body p) 
  (caddr-handler p "PROCEDURE_BODY"))

(define (procedure-environment p) 
  (cadddr-handler p "PROCEDURE_ENVIRONMENT"))

(define (enclosing-environment env)
  (cdr-handler env "ENCLOSING_ENVIRONMENT"))

(define (first-frame env)
  (car-handler env "FIRST_FRAME"))

(define (rest-frames env)
  (cdr-handler env "REST_FRAMES"))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car-handler frame "FRMAE_VARIABLES"))

(define (frame-values frame)
  (cdr-handler frame "FRAME_VALUES"))

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
    (define-variable! '*unassigned* 
                      (lambda () (error "ERROR: unassigned value is used")) initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr-handler proc "PRIMITIVE_IMPLEMENTION"))

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
  (map (lambda (x) (car-handler x "PRIMITIVE_PROCEDURE_NAMES")) 
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
    (let ((output 
            (actual-value 
              input the-global-environemt)))
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
    (error "MAKE_LET")
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

(define (env-loop target env eq-proc error-message)
  (if (eq? env the-empty-environment)
    (error error-message target)
    (let ((frame (first-frame env)))
      (scan 
        target
        (frame-variables frame)
        (frame-values frame)
        (lambda () (env-loop 
                     target 
                     (enclosing-environment env) 
                     eq-proc 
                     error-message))
        eq-proc))))

(define (scan target vars vals null-proc eq-proc)
  (cond ((null? vars) (null-proc))
        ((eq? target (car vars)) (eq-proc vars vals))
        (else (scan target (cdr vars) (cdr vals) null-proc eq-proc))))

(define (set-variable-value! var val env)
  (env-loop 
    var 
    env 
    (lambda (vars vals) (set-car! vals val))
    "Unbound variables: SET!"))
  
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan var
          (frame-variables frame)
          (frame-values frame)
          (lambda () (add-binding-to-frame! var val frame))
          (lambda (vars vals) (set-car! vals val)))))

;;;;; ex4.13 ;;;;;

(define (unbound? exp)
  (tagged-list? exp 'make-unbound!))

(define (unbound-variable exp)
  (cadr-handler exp "UNBOUND_VARIABLE"))

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (scan var
          (frame-variables frame)
          (frame-values frame)
          (lambda () (error "the variable has not defined yet" var))
          (lambda (vars vals) (begin (set-car! vars (cddr vars))
                                     (set-car! vals (cddr vals))
                                     'ok)))))

(define (eval-unbound exp env)
  (make-unbound! (unbound-variable exp) env))

;;;;; ex.4.16 ;;;;;

(define (lookup-variable-value var env)
  (env-loop 
    var 
    env 
    (lambda (vars vals)
      (let ((target (car vars)))
        (if (eq? "*unassigned*" target)
          (error "unassigned value" target)
          (car vals))))
    "Unbound variables"))

(define (scan-out-defines init-l-body)
  (define (scan-let-body l-body definitions bodies)
    (cond ((null? l-body) 
           (if (null? definitions)
             init-l-body
             (definition->let (reverse definitions) (reverse bodies))))
          ((definition? (car l-body))
           (scan-let-body (cdr l-body) 
                          (cons (car l-body) definitions)
                          bodies))
          (else (scan-let-body (cdr l-body) 
                               definitions
                               (cons (car l-body) bodies)))))
  (scan-let-body init-l-body '() '()))

(define (definition->let definitions bodies)
  (define (val-iter defs)
    (if (null? defs)
      bodies
      (cons (make-assignment (definition-variable (car defs)) (definition-value (car defs)))
            (val-iter (cdr defs)))))
  (list 
    (make-let (map (lambda (def) (list (definition-variable def) '*unassigned*))
                   definitions)
              (make-begin (val-iter definitions)))))

;;;;; ex4.17 ;;;;;;

(define (make-assignment variable value)
  (list 'set! variable value))

;;;;; 4.2 ;;;;;

(define (eval exp env)
  (stack-trace "EVAL" exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (scan-out-defines (lambda-body exp))
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((and? exp) (eval-and (and-conjuncts exp) env))
        ((or? exp) (eval-or (or-conjuncts exp) env))
        ((unbound? exp) (eval-unbound exp env))
        ((application? exp)
         (myapply (actual-value (operator exp) env)
                  (operands exp)
                  env))
        (else
          (error "Unknown expression type: eval" exp))))

(define (myapply procedure arguments env)
  (stack-trace "APPLY" procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
           procedure 
           (list-of-args-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-args-values arguments env)
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type: myapply" procedure))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-args-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps)
                        env)
          (list-of-args-values (rest-operands exps)
                               env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delayed-it (first-operand exps)
                      env)
          (list-of-delayed-args (rest-operands exps)
                                env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (delayed-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env))))
           (set-car! obj 'evaluted-thunk)
           (set-car! (cdr obj)
                     result)
           (set-cdr! (cdr obj)
                     '())
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

;;;; eval ;;;;;
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
      (let ((val (actual-value in the-global-environemt)))
        (if (not (eq? 'ok val))
          (begin (display val)
                 (display "\n")
                 (eval-iter))
          (eval-iter)))
      (display ""))))

(eval-iter) 
