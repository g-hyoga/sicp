#!bin/bash

function  run_test () {
  output=$(racket interpreter.rkt $1 $2)
  if [ ! $output = $2 ]; then
    echo "Test Failed"
    echo "expcted: $2"
    echo "actucal: $output"
  fi
}



