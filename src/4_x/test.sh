#!bin/bash

void=$(racket interpreter.rkt "(define void 0)")

function  run_test () {
  output=$(racket interpreter.rkt "$1")
  if [ "$output" != "$2" ]; then
    echo
    echo "Test Failed"
    echo "Test Case: \"$1\", $2"
    echo "expcted: $2"
    echo "actucal: $output"
    echo
  else 
    echo "Test Pass"
  fi
}

run_test "(define hoge 1)" $void
run_test '(define hoge "string")' $void
run_test "(+ 1 2 3)" 6

run_test "(define hoge 1) hoge" 1
