module builder

import v.parser
import v.pref
import v.util
import v.gen.native
import v.markused

pub fn (mut b Builder) build_native(v_files []string, out_file string) {
	$if !linux && !macos {
		eprintln('Warning: v -native can only generate macOS and Linux binaries for now')
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
	util.timing_start('Native GEN')
	b.stats_lines, b.stats_bytes = native.gen(b.parsed_files, b.table, out_file, b.pref)
	util.timing_measure('Native GEN')
}

pub fn (mut b Builder) compile_native() {
	// v.files << v.v_files_from_dir(os.join_path(v.pref.vlib_path,'builtin','bare'))
	files := [b.pref.path]
	if b.pref.arch == ._auto {
		b.pref.arch = pref.get_host_arch()
	}
	b.set_module_lookup_paths()
	b.build_native(files, b.pref.out_name)
}
