module builder

import time
import os
import v.parser
import v.pref
import v.gen
import v.gen.js

pub fn (mut b Builder) gen_js(v_files []string) string {
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
	b.print_warnings_and_errors()
	res := js.gen(b.parsed_files, b.table, b.pref)
	t3 := time.ticks()
	gen_time := t3 - t2
	b.info('JS GEN: ${gen_time}ms')
	return res
}

pub fn (mut b Builder) build_js(v_files []string, out_file string) {
	b.out_name_js = out_file
	b.info('build_js($out_file)')
	output := b.gen_js(v_files)
	mut f := os.create(out_file) or {
		panic(err)
	}
	f.writeln(output)
	f.close()
}

pub fn (mut b Builder) compile_js() {
	mut files := b.get_user_files()
	files << b.get_builtin_files()
	b.set_module_lookup_paths()
	if b.pref.is_verbose {
		println('all .v files:')
		println(files)
	}
	b.build_js(files, b.pref.out_name + '.js')
}

fn (mut b Builder) run_js() {
	cmd := 'node ' + b.pref.out_name + '.js'
	res := os.exec(cmd) or {
		println('JS compilation failed.')
		verror(err)
		return
	}
	println(res.output)
}