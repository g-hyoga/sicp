#lang racket 
(require "./interpreter.rkt")

(define (no-operands? ops) (null? ops))

(define (lr-list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first (first-operand exps))
          (rest (rest-operands exps)))
      (cons (eval first env) 
            (lr-list-of-values rest env)))))

(define (rl-list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first (first-operand exps))
          (rest (rest-operands exps)))
      (cons (eval first env) 
            (rl-list-of-values rest env)))))

