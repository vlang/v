module builder

import (
	time
	os
	v.parser
	v.pref
	v.gen
	v.gen.x64
)

pub fn (b mut Builder) build_x64(v_files []string, out_file string) {
	$if !linux {
		println('v -x64 can only generate Linux binaries for now')
		println('You are not on a Linux system, so you will not ' + 'be able to run the resulting executable')
	}
	t0 := time.ticks()
	b.parsed_files = parser.parse_files(v_files, b.table, b.pref, b.global_scope)
	b.parse_imports()
	t1 := time.ticks()
	parse_time := t1 - t0
	b.info('PARSE: ${parse_time}ms')
	b.checker.check_files(b.parsed_files)
	t2 := time.ticks()
	check_time := t2 - t1
	b.info('CHECK: ${check_time}ms')
	x64.gen(b.parsed_files, out_file)
	t3 := time.ticks()
	gen_time := t3 - t2
	b.info('x64 GEN: ${gen_time}ms')
}

pub fn (b mut Builder) compile_x64(pref &pref.Preferences) {
	// v.files << v.v_files_from_dir(os.join_path(v.pref.vlib_path,'builtin','bare'))
	files := [pref.path]
	b.set_module_lookup_paths()
	b.build_x64(files, pref.out_name)
}