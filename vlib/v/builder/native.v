module builder

import v.pref
import v.util
import v.gen.native
import os

pub fn compile_native(mut b Builder) {
	// v.files << v.v_files_from_dir(os.join_path(v.pref.vlib_path,'builtin','bare'))
	files := [b.pref.path]
	b.set_module_lookup_paths()
	b.build_native(files, b.pref.out_name)
}

pub fn (mut b Builder) build_native(v_files []string, out_file string) {
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
	b.stats_lines, b.stats_bytes = native.gen(b.parsed_files, b.table, out_file, b.pref)
	util.timing_measure('Native GEN')
}
