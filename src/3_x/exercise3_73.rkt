#lang racket
(require racket/stream)

; stream: Stream[number]
; factor: number
; return: Stream[number]
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

; s1: Stream[number]
; s2: Stream[number]
; return: Stream[number]
(define (add-stream s1 s2) (stream-map + s1 s2))

; initial-value + Σ(integrand * dt)
; integrand: Stream[number]
; initial-value: number
; dt: number
; return: Stream[number]
(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-stream (scale-stream integrand dt)
                 int)))
  int)

; r: number 
; c: number
; interval: number
; return: (Stream[number], number) => Stream[number]
(define (RC r c interval)
  (lambda (s initial-v)
    (add-stream
      (scale-stream s r)
      (integral
        (scale-stream s (/ 1 c))
        initial-v
        interval))))

(define RC1 (RC 5 1 0.5))
RC1
