module jsbuilder

import os
import v.pref
import v.util
import v.builder
import v.gen.js

pub fn start() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	builder.compile('build', prefs, compile_js)
}

pub fn compile_js(mut b builder.Builder) {
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	if b.pref.is_verbose {
		println('all .v files:')
		println(files)
	}
	mut name := b.pref.out_name
	if !name.ends_with('.js') {
		name += '.js'
	}
	build_js(mut b, files, name)
}

pub fn build_js(mut b builder.Builder, v_files []string, out_file string) {
	b.out_name_js = out_file
	b.info('build_js(${out_file})')
	output := gen_js(mut b, v_files)
	os.write_file(out_file, output) or { panic(err) }
	if b.pref.is_stats {
		b.stats_lines = output.count('\n') + 1
		b.stats_bytes = output.len
	}
}

pub fn gen_js(mut b builder.Builder, v_files []string) string {
	b.front_and_middle_stages(v_files) or { return '' }
	util.timing_start('JS GEN')
	res := js.gen(b.parsed_files, b.table, b.pref)
	util.timing_measure('JS GEN')
	return res
}
