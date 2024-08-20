#!/usr/bin/env bash

## Note: radamsa is a fuzzer, available from https://gitlab.com/akihe/radamsa

export VFUZZER=true
export OUTPUT_FILE=${1:-x.v}
export PARSER_EXECUTABLE=${2:-cmd/tools/measure/parser_speed}

echo "Fuzzing parameters | OUTPUT FILE: ${OUTPUT_FILE} | PARSER_EXECUTABLE: ${PARSER_EXECUTABLE}"

if [ ! -f $PARSER_EXECUTABLE ]; then
	v -g -o "${PARSER_EXECUTABLE}" cmd/tools/measure/parser_speed.v
fi	

while true; do
	radamsa --meta "${OUTPUT_FILE}.autofuzz.log" examples/hello_world.v > "${OUTPUT_FILE}";
	echo -ne "OFILE: ${OUTPUT_FILE}"; ./"${PARSER_EXECUTABLE}" ${OUTPUT_FILE} || break;
done
