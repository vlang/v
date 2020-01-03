module main

import (
	v.parser
	v.table
	v.gen
	os
)

const (
cdefs = '
#define true 1
#define false 0
typedef int bool;
typedef struct { char* str; } string;
typedef double f64;
string tos3(char* s) { return (string){ .str = s, len = strlen(s) }; }
')

fn main() {
	path := os.args[1]
	println('V2 $path')
	table := table.new_table()
	program := parser.parse_file(path, table)
	res := gen.cgen([program], table)
	mut out := os.create('out.c')?
	out.writeln(cdefs)
	out.writeln(res)
	out.close()
	println('out.c generated')
	os.system('cc out.c')
}
