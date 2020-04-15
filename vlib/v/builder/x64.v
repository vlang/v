module builder

import (
	time
	os
	v.ast
	v.parser
	v.pref
	v.gen
	v.gen.x64
)

pub fn (b mut Builder) build_x64(builtin_files, user_files []string, out_file string) {
	$if !linux {
		println('v -x64 can only generate Linux binaries for now')
		println('You are not on a Linux system, so you will not ' + 'be able to run the resulting executable')
	}
	t0 := time.ticks()
	b.builtin_parsed_files = parser.parse_files(builtin_files, b.table, b.pref, b.global_scope)
	b.user_parsed_files = parser.parse_files(user_files, b.table, b.pref, b.global_scope)
	b.parse_imports()
	t1 := time.ticks()
	parse_time := t1 - t0
	b.info('PARSE: ${parse_time}ms')
	mut parsed_files := []ast.File
	parsed_files << b.builtin_parsed_files
	parsed_files << b.import_parsed_files
	parsed_files << b.user_parsed_files
	b.checker.check_files(parsed_files)
	t2 := time.ticks()
	check_time := t2 - t1
	b.info('CHECK: ${check_time}ms')
	x64.gen(parsed_files, out_file)
	t3 := time.ticks()
	gen_time := t3 - t2
	b.info('x64 GEN: ${gen_time}ms')
}

pub fn (b mut Builder) compile_x64(pref &pref.Preferences) {
	builtin_files := []string
	user_files := []string
	// v.files << v.v_files_from_dir(os.join_path(v.pref.vlib_path,'builtin','bare'))
	user_files << pref.path
	b.set_module_lookup_paths()
	b.build_x64(builtin_files, user_files, pref.out_name)
}