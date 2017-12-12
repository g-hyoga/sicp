yishibashi
```scheme

;ex 3.64
(define (stream-limit s t)
 (let ((fst (stream-car s))
       (scd (stream-car (stream-cdr s))))
   (if (< (abs (- fst scd)) t)
     scd
     (stream-limit (stream-cdr s) t))))

;ex 3.65
(define (ln2-summands n)
 (cons-stream (/ 1.0 n)
              (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
 (partial-sums (ln2-summands 1)))

```

---------------------------------------------------------------------

hyo
```racket
exe 3.64
(define (stream-limit s t)
  (let ((first (stream-first s))
          (second (stream-first (stream-rest s))))
    (if (< (abs (- second first)) t)
      second
      (stream-limit (stream-rest s) t))))

exe 3.65
(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2 (+ n 1)))))
(define ln2-stream
  (partical-sums (ln2-summands 1)))

(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1))
          (s2 (stream-ref s 2)))
      (stream-cons (- s2 (/ (square (- s2 s1)))
                      (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-rest s))))

(euler-transform ln2)
```

---------------------------------------------------------------------

-eudika
3.64

    (define (stream-limit stream tolerance)
        (let   ((t1 (stream-car stream))
            (t2 (stream-car (stream-cdr stream))))
            (if (< (abs (- t1 t2)) tolerance) t2
                (stream-limit (stream-cdr stream) tolerance))))

3.65

    (define (ln2-summands n)
        (stream-cons (/ 1 n) (stream-map - (ln2-summands (+ n 1)))))
    (define ln2-stream
        (partial-sums (ln2-summans 1)))
Add Comment
