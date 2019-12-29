module main

import (
	v.parser
	v.table
	v.cgen
	os
)

fn main() {
	path := os.args[1]
	println('V2 $path')
	text := os.read_file(path)?
	table := &table.Table{}
	program := parser.parse_file(text, table)
	res := cgen.gen(program)
	mut out := os.create('out.c')?
	out.writeln(res)
	out.close()
}
