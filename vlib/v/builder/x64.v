module builder

import v.parser
import v.pref
import v.util
import v.gen.x64
import v.markused

pub fn (mut b Builder) build_x64(v_files []string, out_file string) {
	$if !linux {
		println('v -x64 can only generate Linux binaries for now')
		println('You are not on a Linux system, so you will not ' +
			'be able to run the resulting executable')
	}
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
	util.timing_start('x64 GEN')
	x64.gen(b.parsed_files, b.table, out_file, b.pref)
	util.timing_measure('x64 GEN')
}

pub fn (mut b Builder) compile_x64() {
	// v.files << v.v_files_from_dir(os.join_path(v.pref.vlib_path,'builtin','bare'))
	files := [b.pref.path]
	b.set_module_lookup_paths()
	b.build_x64(files, b.pref.out_name)
}
