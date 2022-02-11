module cbuilder

import os
import v.pref
import v.util
import v.builder
import v.gen.c

pub fn start() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	builder.compile('build', prefs, compile_c)
}

pub fn compile_c(mut b builder.Builder) {
	if b.pref.is_verbose {
		println('all .v files before:')
	}
	$if windows {
		b.find_win_cc() or { builder.verror(builder.no_compiler_error) }
	}
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	if b.pref.is_verbose {
		println('all .v files:')
		println(files)
	}
	mut out_name_c := b.get_vtmp_filename(b.pref.out_name, '.tmp.c')
	if b.pref.is_shared {
		out_name_c = b.get_vtmp_filename(b.pref.out_name, '.tmp.so.c')
	}
	build_c(mut b, files, out_name_c)
	b.cc()
}

pub fn gen_c(mut b builder.Builder, v_files []string) string {
	b.front_and_middle_stages(v_files) or {
		if err.code() != 9999 {
			builder.verror(err.msg())
		}
		return ''
	}

	util.timing_start('C GEN')
	res := c.gen(b.parsed_files, b.table, b.pref)
	util.timing_measure('C GEN')
	return res
}

pub fn build_c(mut b builder.Builder, v_files []string, out_file string) {
	b.out_name_c = out_file
	b.pref.out_name_c = os.real_path(out_file)
	b.info('build_c($out_file)')
	output2 := gen_c(mut b, v_files)
	os.write_file(out_file, output2) or { panic(err) }
	if b.pref.is_stats {
		b.stats_lines = output2.count('\n') + 1
		b.stats_bytes = output2.len
	}
}
