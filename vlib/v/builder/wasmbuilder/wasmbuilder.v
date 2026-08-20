module wasmbuilder

import v.pref
import v.util
import v.builder
import v.gen.wasm

pub fn start() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	if report := builder.take_external_v3_report_from_env() {
		// A V3->V1 fallback staged a report before launching this builder. Route it
		// through compile_with_external_c_error_report so the notice/submission and
		// the staged-directory cleanup happen relative to THIS build's outcome (only
		// notify/submit after it succeeds), instead of before the retry ran.
		builder.compile_with_external_c_error_report('build', prefs, compile_wasm, report)
	} else {
		builder.compile('build', prefs, compile_wasm)
	}
}

pub fn compile_wasm(mut b builder.Builder) {
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	if b.pref.is_verbose {
		println('all .v files:')
		println(files)
	}
	mut name := b.pref.out_name
	if name.ends_with('/-') || name.ends_with(r'\-') || name == '-' {
		name = '-'
	} else if !name.ends_with('.wasm') {
		name += '.wasm'
	}
	build_wasm(mut b, files, name)
}

pub fn build_wasm(mut b builder.Builder, v_files []string, out_file string) {
	b.front_and_middle_stages(v_files) or { return }
	util.timing_start('WebAssembly GEN')
	wasm.gen(b.parsed_files, mut b.table, out_file, b.pref)
	util.timing_measure('WebAssembly GEN')
}
