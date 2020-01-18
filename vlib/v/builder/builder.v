module builder

import (
	os
	time
	v.table
	v.checker
	v.parser
	v.gen
	v.gen.x64
)

pub struct Builder {
pub:
	table   &table.Table
	checker checker.Checker
}

pub fn new_builder() Builder {
	table := table.new_table()
	return Builder{
		table: table
		checker: checker.new_checker(table)
	}
}

pub fn (b mut Builder) gen_c(v_files []string) string {
	ast_files := parser.parse_files(v_files, b.table)
	b.checker.check_files(v_files, ast_files)
	return gen.cgen(ast_files, b.table)
}

pub fn (b mut Builder) build_c(v_files []string, out_file string) {
	os.write_file(out_file, b.gen_c(v_files))
}

pub fn (b mut Builder) build_x64(v_files []string, out_file string) {
	ticks := time.ticks()
	ast_files := parser.parse_files(v_files, b.table)
	println('PARSE: ${time.ticks() - ticks}ms')
	b.checker.check_files(v_files, ast_files)
	println('CHECK: ${time.ticks() - ticks}ms')
	x64.gen(ast_files, out_file)
	println('x64 GEN: ${time.ticks() - ticks}ms')
}
