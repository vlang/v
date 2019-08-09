#!/bin/sh

score=0
total=0
debug=0

if [ "$1" = "-h" ]; then
  echo "V REPL Tests Script"
  echo
  echo "-d for debug extra outputs"
  echo
  echo "Reads test files '*.repl_test' and execute them"
  echo "See README.md for details on how to write tests"
  exit 0
fi

if [ "$1" = "-d" ]; then
    debug=1
fi

test_command() {
  if [ $debug -eq 1 ]; then
    echo -en "Testing $1: "
  fi

  file=`cat $1`
  output_start=`echo -e "$file" | grep -n '===output===' | cut -f1 -d:`

  if [ $output_start -gt 1 ]; then
    input=`echo -en "$file" | head -n $((output_start - 1))`
  else
    input=""
  fi

  output=`echo -en "$file" | tail -n +$((output_start + 1))`
  result=`echo -en "$input" | ./v | tail -n +3 | sed 's/>>> //g'`

  if [ "$output" = "$result" ]; then
    score=$(($score + 1))
    if [ $debug -eq 1 ]; then
      echo -e "\033[32mOK\033[0m"
    fi
  else
    if [ $debug -eq 0 ]; then
      echo -en "REPL: "
    fi
    echo -e "\033[31mKO\033[0m"
    echo -e "\033[1mGot :\033[0m\n$result"
    echo -e "\033[1mExpected :\033[0m\n$output"
  fi

  total=$(($total + 1))
  rm -f .vrepl .vrepl_temp
}

for file in `ls compiler/tests/repl/*.repl`; do
  test_command $file
done

echo -e "\033[1mREPL SCORE:" $score "/" $total "\033[0m"

if [ $score != $total ]; then
  exit 1
fi
