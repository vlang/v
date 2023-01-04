module nativebuilder

import os
import v.pref
import v.util
import v.builder
import v.gen.native

pub fn start() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	builder.compile('build', prefs, compile_native)
}

pub fn compile_native(mut b builder.Builder) {
	// v.files << v.v_files_from_dir(os.join_path(v.pref.vlib_path,'builtin','bare'))
	files := [b.pref.path]
	b.set_module_lookup_paths()
	build_native(mut b, files, b.pref.out_name)
}

pub fn build_native(mut b builder.Builder, v_files []string, out_file string) {
	if b.pref.os == .windows {
		eprintln('Warning: v -native is experimental for Windows')
		if !b.pref.is_shared && b.pref.build_mode != .build_module
			&& !b.pref.out_name.ends_with('.exe') {
			b.pref.out_name += '.exe'
		}
	} else if b.pref.os !in [.linux, .macos] {
		eprintln('Warning: v -native can only generate macOS and Linux binaries for now')
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
	util.timing_start('Native GEN')
	if b.pref.arch == ._auto {
		$if amd64 {
			b.pref.arch = .amd64
		} $else $if arm64 {
			b.pref.arch = .arm64
		} $else {
			eprintln('Error: Only arm64 and amd64 are supported by V')
		}
	}
	b.stats_lines, b.stats_bytes = native.gen(b.parsed_files, b.table, out_file, b.pref)
	util.timing_measure('Native GEN')
}
