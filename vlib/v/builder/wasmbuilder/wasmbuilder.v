module wasmbuilder

import os
import v.pref
import v.util
import v.builder
import v.gen.wasm

pub fn start() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	builder.compile('build', prefs, compile_wasm)
}

pub fn compile_wasm(mut b builder.Builder) {
	// v.files << v.v_files_from_dir(os.join_path(v.pref.vlib_path,'builtin','bare'))
	files := [b.pref.path]
	b.set_module_lookup_paths()
	build_wasm(mut b, files, b.pref.out_name)
}

pub fn build_wasm(mut b builder.Builder, v_files []string, out_file string) {
	b.pref.out_name_c = b.pref.out_name
	if !b.pref.out_name.ends_with('.wasm') {
		b.pref.out_name += '.wasm'
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
	util.timing_start('WebAssembly GEN')
	wasm.gen(b.parsed_files, b.table, out_file, b.pref)
	util.timing_measure('WebAssembly GEN')
}
