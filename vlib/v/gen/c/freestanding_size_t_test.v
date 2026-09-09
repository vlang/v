module c

import os
import v.ast
import v.checker
import v.parser
import v.pref

fn generate_freestanding_output(main_src string) string {
	mut table := ast.new_table()
	mut pref_ := pref.new_preferences()
	pref_.is_bare = true
	builtin_src := 'module builtin\n\nasm amd64 {\n\t.globl _start\n\t_start:\n\tret\n}\n'
	mut builtin_file := parser.parse_text(builtin_src, os.join_path('/virtual', 'vlib', 'builtin',
		'linux_bare', 'linux_syscalls.v'), mut table, .skip_comments, pref_)
	mut main_file := parser.parse_text(main_src, os.join_path('/virtual', 'main.v'), mut table,
		.skip_comments, pref_)
	mut chk := checker.new_checker(table, pref_)
	chk.check(mut builtin_file)
	chk.check(mut main_file)
	result := gen([builtin_file, main_file], mut table, pref_)
	return result.res_builder.bytestr()
}

fn test_freestanding_c_output_does_not_redeclare_size_t() {
	generated := generate_freestanding_output('module main\n\nfn main() {}\n')
	assert generated.contains('<stddef.h>')
	assert !generated.contains('typedef long unsigned int size_t;')
}
