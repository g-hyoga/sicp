#lang racket

(require racket/stream)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (interleave s1 s2)
  (if (stream-empty? s1)
    s2
    (stream-cons (stream-first s1)
                 (interleave s2 (stream-rest s1)))))

; s1: stream[A]
; s2: stream[A]
; weight: A => int
; return: stream[A]
(define (merge-weighted s1 s2 weight)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else 
          (let ((s1car (stream-first s1))
                (s2car (stream-first s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (stream-cons
                     s1car
                     (merge-weighted (stream-rest s1) s2 weight)))
                  ((> (weight s1car) (weight s2car))
                   (stream-cons
                     s2car
                     (merge-weighted s1 (stream-rest s2) weight)))
                  ((and (or (not (= (car s1car) (car s2car))) (not (= (cadr s1car) (cadr s2car))))
                        (= (weight s1car) (weight s2car)))
                   (stream-cons
                     s1car
                     (stream-cons
                       s2car
                       (merge-weighted (stream-rest s1) (stream-rest s2) weight))))
                  (else
                    (stream-cons
                      s1car
                      (merge-weighted (stream-rest s1) (stream-rest s2) weight))))))))

(define (pairs s t)
  (stream-cons
    (list (stream-first s) (stream-first t))
    (interleave
      (stream-map (lambda (x) (list (stream-first s) x))
                  (stream-rest t))
      (pairs (stream-rest s) (stream-rest t)))))

; s: stream[A]
; t: stream[A]
; weight: list[A] => int
; return: stream[list[A]]
(define (weighted-pairs s t weight)
  (let ((ps (pairs s t)))
    (merge-weighted ps ps weight)))

(define (integer-starting-from n)
  (stream-cons n (integer-starting-from (+ n 1))))

(define integers (integer-starting-from 1))

(define (ramanujan)
  (define (weight pair)
    (+ (* (car pair) (car pair) (car pair))
                        (* (cadr pair) (cadr pair) (cadr pair))))
  (define cube 
    (weighted-pairs
      integers
      integers
      weight))
  (define (iter c)
    (let* ((rest (stream-rest c))
           (first (stream-first c))
           (second (stream-first rest)))
      (if (= (weight first) (weight second))
        (stream-cons second (iter rest))
        (iter rest))))
  (iter cube))

(define r (ramanujan))

(stream-first r)
;(stream-first (stream-rest r))
;(stream-first (stream-rest (stream-rest r)))
;(stream-first (stream-rest (stream-rest (stream-rest r))))
;(stream-first (stream-rest (stream-rest (stream-rest (stream-rest r)))))
(display "\n")
