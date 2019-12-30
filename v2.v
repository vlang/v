module main

import (
	v.parser
	v.table
	v.cgen
	os
)

const (
cdefs = '
#define true 1
#define false 0
typedef struct { char* str; } string;
typedef double f64;
string tos3(char* s) { return (string){ .str = s }; }
')

fn main() {
	path := os.args[1]
	println('V2 $path')
	text := os.read_file(path)?
	table := &table.Table{}
	program := parser.parse_file(text, table)
	res := cgen.gen(program)
	mut out := os.create('out.c')?
	out.writeln(cdefs)
	out.writeln(res)
	out.close()
	println('out.c generated')
	os.system('cc out.c')
}
