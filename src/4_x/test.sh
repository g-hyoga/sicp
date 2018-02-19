#!bin/bash

function  run_test () {
  output=$(racket interpreter.rkt "$1")
  if [ "$output" != "$2" ]; then
    echo "Test Failed"
    echo "expcted: $2"
    echo "actucal: $output"
  else 
    echo "Test Pass"
  fi

}

run_test "(define hoge 1) hoge" ok

