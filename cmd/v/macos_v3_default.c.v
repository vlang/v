module main

import os
import v.pref
import v.util

// On platforms where V3 is not the default compiler, `v` still runs the V1
// compiler normally. `-new-compiler` opts into the embedded V3 driver and runs
// it in THIS process, the same way V3 runs by default on macOS.
fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) ?MacosV3CErrorReport {
	if prefs.new_compiler {
		run_new_compiler_in_process(command, prefs)
	}
	_ = command
	return none
}

// run_new_compiler_in_process compiles with the embedded V3 driver (vlib/v3) in
// the current process. It never launches a separate `v3` executable and never
// falls back to V1, so a V3 failure is reported as-is.
fn run_new_compiler_in_process(command string, prefs &pref.Preferences) {
	if !macos_v3_driver_is_available() {
		eprintln('`-new-compiler` requires a build that embeds the V3 compiler, which this one does not.')
		exit(1)
	}
	raw_args := util.join_env_vflags_and_os_args()[1..]
	if macos_v3_has_v1_only_leading_option(raw_args, command) || v3_has_v1_only_preferences(prefs)
		|| (prefs.gc_set_by_flag && prefs.gc_mode != .no_gc) || (prefs.autofree && prefs.is_run) {
		eprintln('`-new-compiler` cannot be combined with a V1-only option or mode; drop `-new-compiler` or the option.')
		exit(1)
	}
	vexe := pref.vexe_path()
	util.set_vroot_folder(os.dir(vexe))
	os.setenv('VCHILD', 'true', true)
	os.setenv('VEXE', os.real_path(vexe), true)
	forwarded := macos_v3_forwarded_args(prefs, raw_args)
	if prefs.is_verbose {
		println('Running V3 compiler in process: ${util.args_quote_paths(forwarded)}')
	}
	macos_v3_driver_run(forwarded)
	exit(0)
}
