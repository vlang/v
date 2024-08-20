#!/usr/bin/env bash

## Note: radamsa is a fuzzer, available from https://gitlab.com/akihe/radamsa

## ./v -g cmd/tools/measure/parser_speed.v

while true; do 
	radamsa --meta autofuzz.log examples/hello_world.v > x.v; 
	VFUZZER=true cmd/tools/measure/parser_speed x.v || break; 
done
