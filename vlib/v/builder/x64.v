module builder

import time
import v.parser
import v.pref
import v.gen
import v.gen.x64

pub fn (mut b Builder) build_x64(v_files []string, out_file string) {
	$if !linux {
		println('v -x64 can only generate Linux binaries for now')
		println('You are not on a Linux system, so you will not ' + 'be able to run the resulting executable')
	}
	t0 := time.ticks()
	b.parsed_files = parser.parse_files(v_files, b.table, b.pref, b.global_scope)
	b.parse_imports()
	t1 := time.ticks()
	parse_time := t1 - t0
	b.timing_message('PARSE', parse_time)
	b.checker.check_files(b.parsed_files)
	t2 := time.ticks()
	check_time := t2 - t1
	b.timing_message('CHECK', check_time)
	x64.gen(b.parsed_files, b.table, out_file, b.pref)
	t3 := time.ticks()
	gen_time := t3 - t2
	b.timing_message('x64 GEN', gen_time)
}

pub fn (mut b Builder) compile_x64() {
	// v.files << v.v_files_from_dir(os.join_path(v.pref.vlib_path,'builtin','bare'))
	files := [b.pref.path]
	b.set_module_lookup_paths()
	b.build_x64(files, b.pref.out_name)
}
