#lang racket
(require racket/stream)

; li: list[A]
; return: Stream[A]
(define (list->stream li)
  (define (iter li s)
    (if (null? li)
      s
      (iter (cdr li) (stream-cons (car li) s))))
  (iter (reverse li) empty-stream))

; proc: (A, A, ..) => A
; argstreams: List[Stream[A]]
; return: Stream[A]
(define (stream-map proc . argstreams)
	(if (stream-empty? (car argstreams))
		empty-stream
		(stream-cons
			(apply proc (map stream-first argstreams))
			(apply stream-map
						 (cons proc (map stream-rest argstreams))))))

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
; delayed-integrand: delay[Stream[number]]
; initial-value: number
; dt: number
; return: Stream[number]
;
; (define (integral delayed-integrand initial-value dt)
;   (define int
;     (stream-cons initial-value
;                  (let ((integrand (force delayed-integrand)))
;                    (add-stream (scale-stream integrand dt)
;                                int))))
;   int)
(define (integral delayed-integrand initial-value dt)
  (stream-cons
    initial-value
    (let ((integrand (force delayed-integrand)))
      (if (stream-empty? integrand)
        empty-stream
        (integral (stream-rest integrand)
                  (+ (* dt (stream-first integrand))
                     initial-value)
                  dt)))))

; f: (Stream[number]) => Stream[number]
; y0: number
; dt: number
; return: Stream[number]
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; a: number
; b: number
; dt: number
; y0: number
; dy0: number
; return: Stream[number]
(define (solve-2nd f y0 dt))

(stream-ref (solve (lambda (y) y)
                   1
                   0.001)
            1000)


