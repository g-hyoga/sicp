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
                  (else
                    (stream-cons
                      s1car
                      (merge-weighted (stream-rest s1) (stream-rest s2) weight))))))))

; s: stream[int]
; t: stream[int]
; return: stream[list[int]]
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

(define a (weighted-pairs 
            integers 
            integers 
            (lambda (pair) (+ (car pair) (cadr pair)))))

(stream-first a)
(stream-first (stream-rest a))
(stream-first (stream-rest (stream-rest a)))
(stream-first (stream-rest (stream-rest (stream-rest a))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest a)))))
(display "\n")


(define (b? x)
  (not (or (= (remainder x 2) 0)
           (= (remainder x 3) 0)
           (= (remainder x 5) 0))))

(define b-stream
  (stream-filter b? integers))

(define b (weighted-pairs
            b-stream
            b-stream
            (lambda (pair) (+ (* 2 (car pair))
                            (* 3 (cadr pair))
                            (* 5 (car pair) (cadr pair))))))

(stream-first b)
(stream-first (stream-rest b))
(stream-first (stream-rest (stream-rest b)))
(stream-first (stream-rest (stream-rest (stream-rest b))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest b)))))
(display "\n")

