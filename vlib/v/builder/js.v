module builder

import os
import v.pref
import v.util
import v.gen.js

pub fn compile_js(mut b Builder) {
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
	b.build_js(files, name)
}

pub fn (mut b Builder) gen_js(v_files []string) string {
	b.front_and_middle_stages(v_files) or { return '' }
	util.timing_start('JS GEN')
	res := js.gen(b.parsed_files, b.table, b.pref)
	util.timing_measure('JS GEN')
	return res
}

pub fn (mut b Builder) build_js(v_files []string, out_file string) {
	b.out_name_js = out_file
	b.info('build_js($out_file)')
	output := b.gen_js(v_files)
	os.write_file(out_file, output) or { panic(err) }
	if b.pref.is_stats {
		b.stats_lines = output.count('\n') + 1
		b.stats_bytes = output.len
	}
}

fn (mut b Builder) run_js() {
	cmd := 'node ' + b.pref.out_name + '.js'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('JS compilation failed:')
		verror(res.output)
		return
	}
	println(res.output)
}
