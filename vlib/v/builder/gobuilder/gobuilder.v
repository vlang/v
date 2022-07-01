module gobuilder

import os
import v.pref
import v.util
import v.builder
import v.gen.golang

pub fn start() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	builder.compile('build', prefs, compile_go)
}

pub fn compile_go(mut b builder.Builder) {
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	if b.pref.is_verbose {
		println('all .v files:')
		println(files)
	}
	mut name := b.pref.out_name
	if !name.ends_with('.go') {
		name += '.go'
	}
	build_go(mut b, files, name)
}

pub fn build_go(mut b builder.Builder, v_files []string, out_file string) {
	if b.pref.os == .windows {
		if !b.pref.is_shared && b.pref.build_mode != .build_module
			&& !b.pref.out_name.ends_with('.exe') {
			b.pref.out_name += '.exe'
		}
	}
	mut nvf := []string{}
	for vf in v_files {
		if os.is_dir(vf) {
			nvf << b.v_files_from_dir(vf)
		} else {
			nvf << vf
		}
	}
	b.front_and_middle_stages(nvf) or { return }
	util.timing_start('Golang GEN')
	b.stats_lines, b.stats_bytes = golang.gen(b.parsed_files, b.table, out_file, b.pref)
	util.timing_measure('Golang GEN')
}
