module builder

import os
import v.parser
import v.pref
import v.util
import v.gen.js
import v.markused

pub fn (mut b Builder) gen_js(v_files []string) string {
	util.timing_start('PARSE')
	b.parsed_files = parser.parse_files(v_files, b.table, b.pref, b.global_scope)
	b.parse_imports()
	util.get_timers().show('SCAN')
	util.get_timers().show('PARSE')
	util.get_timers().show_if_exists('PARSE stmt')
	//
	util.timing_start('CHECK')
	b.checker.check_files(b.parsed_files)
	util.timing_measure('CHECK')
	//
	if b.pref.skip_unused {
		markused.mark_used(mut b.table, b.pref, b.parsed_files)
	}
	b.print_warnings_and_errors()
	//
	util.timing_start('JS GEN')
	res := js.gen(b.parsed_files, b.table, b.pref)
	util.timing_measure('JS GEN')
	return res
}

pub fn (mut b Builder) build_js(v_files []string, out_file string) {
	b.out_name_js = out_file
	b.info('build_js($out_file)')
	output := b.gen_js(v_files)
	mut f := os.create(out_file) or { panic(err) }
	f.writeln(output) or { panic(err) }
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
	mut name := b.pref.out_name
	if !name.ends_with('.js') {
		name += '.js'
	}
	b.build_js(files, name)
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
