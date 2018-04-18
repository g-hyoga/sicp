#!bin/bash

INTERPRETER_DEBUG_MODE=false
void=$(racket interpreter.rkt "(define void 0)")

function run_test () {
  output=$(racket interpreter.rkt "$1")
  if [ "$output" != "$2" ]; then
    echo
    echo "Test Failed"
    echo "Test Case: \"$1\", $2"
    echo "expected: $2"
    echo "actual: $output"
    echo

    if [ "$TEST_DEBUG_MODE" = "true" ]; then
      echo "TEST LOG"
      INTERPRETER_DEBUG_MODE="true" racket interpreter.rkt "$1"
      echo 
    fi
  else 
    echo "Test Pass"
  fi
}

function error_test () {
  racket interpreter.rkt "$1" > /dev/null 2> tmp
  grep "$2" tmp > /dev/null
  if [ $? != 0 ]; then
    echo 
    echo "Test Failed"
    echo "Error message in this test must include '$2'"
    echo
  else 
    echo "Test Pass"
  fi
  rm tmp
}

run_test "
(define hoge 0)
(set! hoge 1)
(set! hoge 2)
hoge
" 2

run_test "
(define hoge 0)
((lambda (x) (set! x 1) (set! x 2)) hoge)
" $void

run_test "
((lambda () (+ 1 1) (+ 2 3) (+ 4 5)))
" 9

run_test "
(define hoge 0)
((lambda (x) (set! x 1)) hoge)
hoge
" 0

run_test "
((lambda ()
  (define x 1)
  (define y 2)
  (+ x y)))
" 3

error_test "
hoge
" "Unbound variables"

error_test "
(define x 123)
(make-unbound! x)
x
" "Unbound variables"

run_test "
(if false 1 2)
" 2

run_test "
(if true 1 2)
" 1

run_test "
((lambda () (+ 1 1)))
" 2

run_test '
(define x "hoge")
x
' "hoge"

run_test "
(define x 1)
(define x 2)
x
" 2

error_test "
(let fib-iter ((a 1)) a)
fib-iter
" "Unbound variables"

run_test "
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))))
(fib 5)
" 5

run_test "
(define (let*-test)
  (let* ((x 1)
         (y (+ x 1))
         (z (+ x y)))
    z))
(let*-test)
" 3

run_test "
(define (let-test x)
  (let ((y 1))
    (+ y x)))
(let-test 2)
" 3

run_test "(define hoge 1) hoge" 1

run_test "
(cond (1 => (lambda (x) (+ 1 x)))
      (else false))" 2

run_test "(and (= 1 1) (= 1 2))" "#f"
run_test "(and (= 1 1) (< 1 2))" "#t"
run_test '(and (= 1 1) (< 1 2) "hoge")' "hoge"
run_test "(or (= 1 2) (= 1 1))" "#t"
run_test "(or (> 1 2) (= 1 2))" "#f"
run_test "(or 1 (= 1 2))" 1
run_test "(define hoge 1)" $void
run_test '(define hoge "string")' $void
run_test "(+ 1 2 3)" 6

