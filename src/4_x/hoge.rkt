((lambda () 
   (begin (define (fib-iter a b count)
            ((if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
          (fib-iter 1 0 5))))

(define (fib n)
  ((lambda (a b count) 
     (begin (define (fib-iter a b count)
              (if (= count 0)
                b
                (fib-iter (+ a b) a (- count 1))))
            (fib-iter a b count)))
   1 0 n))

これを目指す
(lambda (a b count) 
  (begin (define (fib-iter a b count)
           (if (= count 0)
             b
             (fib-iter (+ a b) a (- count 1))))
         (fib-iter a b count)))

( (lambda (a b count) 
   (begin (define (fib-iter a b count) 
            (begin (if (= count 0) b (fib-iter (+ a b) a (- count 1)))
                   )) 
          (fib-iter a b count))) 1 0 5)

