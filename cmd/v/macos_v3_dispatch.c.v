module main

// This is the default-compiler dispatcher for the platforms where the V3
// compiler (vlib/v3) is embedded and runs in-process: macOS and Linux. On those
// targets `v` hands ordinary compile commands to V3 by default, and silently
// falls back to the stable V1 compiler when V3 cannot build a program yet. The
// file carries no `_darwin`/`_linux` suffix so a single implementation compiles
// everywhere; the actual dispatch is gated at runtime by
// `macos_v3_driver_is_available()`, which is only true on macOS and Linux (see
// macos_v3_driver_notd_cross.v). On Windows/BSD the driver is absent, so this
// dispatcher only rejects an explicit `-new-compiler` and otherwise stays out of
// the way. The `macos_v3_` name is kept for continuity with the original macOS
// rollout even though the behavior now also covers Linux.
import os
import v.pref
import v.util

const macos_v3_fallback_file_env = 'V_MACOS_V3_FALLBACK_FILE'
const macos_v3_c_error_dir_env = 'V_MACOS_V3_C_ERROR_DIR'
const macos_v3_vhash_env = 'V_MACOS_V3_VHASH'
const macos_v3_vcurrent_hash_env = 'V_MACOS_V3_VCURRENT_HASH'
const macos_v3_caller_vexe_env = 'V_MACOS_V3_CALLER_VEXE'
const macos_v3_caller_vexe_present_env = 'V_MACOS_V3_CALLER_VEXE_PRESENT'
const macos_v3_caller_vchild_env = 'V_MACOS_V3_CALLER_VCHILD'
const macos_v3_caller_vchild_present_env = 'V_MACOS_V3_CALLER_VCHILD_PRESENT'
const macos_v3_embedded_env = 'V_MACOS_V3_EMBEDDED'
const macos_v3_retry_env = 'V_MACOS_V3_RETRY'
const macos_v3_no_fallback_env = 'V_MACOS_V3_NO_FALLBACK'
const macos_v3_inline_asm_fallback = 'inline_asm'
const macos_v3_compiler_error_fallback = 'compiler_error'
const macos_v3_c_error_fallback = 'c_compilation_error'
const macos_v3_c_error_compiler_file = 'compiler'
const macos_v3_c_error_output_file = 'output'
const macos_v3_c_error_source_name_file = 'source_name'
// optional marker file inside a staged report dir; when it holds
// `macos_v3_compiler_error_fallback` the report describes a V3 internal compiler
// error (V source only) instead of a generated-C compilation error.
const macos_v3_c_error_kind_file = 'kind'

fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) ?MacosV3CErrorReport {
	if os.getenv(macos_v3_retry_env) == '1' {
		os.unsetenv(macos_v3_retry_env)
		return take_macos_v3_c_error_report()
	}
	if prefs.old_compiler {
		return take_macos_v3_c_error_report()
	}
	if !macos_v3_driver_is_available() {
		if prefs.new_compiler {
			eprintln('`-new-compiler` requires a build that embeds the V3 compiler, which this one does not.')
			exit(1)
		}
		return take_macos_v3_c_error_report()
	}
	all_args := util.join_env_vflags_and_os_args()
	forwarded_args := all_args[1..]
	if !is_macos_v3_default_executable(os.executable()) {
		trace_macos_v3_skip('non-default compiler executable `${os.executable()}`')
		return none
	}
	if macos_v3_has_v1_only_leading_option(forwarded_args, command) {
		if prefs.new_compiler {
			eprintln('`-new-compiler` cannot be combined with a V1-only option; remove it or drop `-new-compiler`.')
			exit(1)
		}
		return none
	}
	if !is_macos_v3_relevant_command(command, prefs) && !macos_v3_force_requested(command, prefs) {
		return none
	}
	return launch_macos_v3_compiler(prefs, forwarded_args)
}

fn trace_macos_v3_skip(reason string) {
	if os.getenv('V3_CACHE_TRACE') != '' {
		eprintln('  macOS V3 dispatch skipped: ${reason}')
	}
}

fn is_macos_v3_default_executable(vexe string) bool {
	return os.base(vexe) in ['v', 'v.exe', 'vnew', 'vnew.exe']
}

fn is_macos_v3_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.old_compiler {
		return false
	}
	if v3_has_v1_only_preferences(prefs) || (prefs.gc_set_by_flag && prefs.gc_mode != .no_gc) {
		// V1 still owns compiler modes whose runtime or C toolchain support has not
		// been implemented by V3 yet. The implicit GC default is resolved before
		// dispatch, but it must not prevent V3 from being the default compiler.
		return false
	}
	if prefs.autofree {
		// V1 owns autofree here. On macOS a direct `v -autofree` build is delegated
		// to the ownership-enabled compiler before this point (so only autofree
		// `run` arrives). On Linux autofree is NOT delegated and the ordinary
		// embedded V3 has no `ownership` support — it would exit with "ownership
		// support is not compiled into this v3 executable" — so keep every autofree
		// build and run on V1 rather than dispatching it to a V3 that cannot honor it.
		return false
	}
	if command == 'test' {
		// Keep discovery, per-file isolation, build constraints, and result
		// aggregation in vtest. Each _test.v file is compiled by this executable
		// again, so user test code still uses V3 by default.
		return false
	}
	if prefs.path == '' {
		return false
	}
	normalized_path := prefs.path.replace('\\', '/').trim_right('/')
	// cmd/v remains the command dispatcher. All other user compilation and test
	// modes use V3 by default.
	if normalized_path == 'cmd/v' || normalized_path.starts_with('cmd/v/')
		|| normalized_path.contains('/cmd/v/') || normalized_path.ends_with('/cmd/v')
		|| normalized_path == 'vlib/v3/v3.v' || normalized_path.ends_with('/vlib/v3/v3.v')
		|| is_macos_v3_internal_tool_bootstrap(normalized_path, os.getenv('VCHILD') == 'true') {
		return false
	}
	if command in external_tools {
		return false
	}
	if macos_v3_non_compilation_command(command) {
		return false
	}
	return command in ['run', 'build', 'test'] || prefs.is_script || os.is_dir(prefs.path)
		|| normalized_path.ends_with('.v') || normalized_path.ends_with('.vsh')
}

fn is_macos_v3_internal_tool_bootstrap(normalized_path string, is_vchild bool) bool {
	return is_vchild
		&& (normalized_path.starts_with('cmd/tools/') || normalized_path.contains('/cmd/tools/'))
}

fn launch_macos_v3_compiler(prefs &pref.Preferences, raw_args []string) ?MacosV3CErrorReport {
	caller_environment := os.environ()
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	util.set_vroot_folder(vroot)
	forwarded_args := macos_v3_forwarded_args(prefs, raw_args)
	if prefs.is_verbose {
		println('Running macOS V3 compiler in process: ${util.args_quote_paths(forwarded_args)}')
	}
	fallback_file := os.join_path(os.vtmp_dir(), 'macos_v3_fallback_${os.getpid()}')
	os.rm(fallback_file) or {}
	c_error_dir := macos_v3_c_error_report_dir(fallback_file)
	os.rmdir_all(c_error_dir) or {}
	mut environment := macos_v3_child_environment(vexe, fallback_file, caller_environment)
	if prefs.new_compiler {
		// The user explicitly asked for V3, so a V3 failure must be reported
		// instead of silently retrying the build with the V1 compiler.
		environment[macos_v3_no_fallback_env] = '1'
	}
	replace_macos_v3_process_environment(environment)
	is_verbose := prefs.is_verbose
	// The input path is captured before V3 runs so the compatibility fallback can
	// stage it for a bug report if V3 fails with an internal compiler error.
	input_path := prefs.path
	retry_args := os.args[1..].clone()
	at_exit(fn [caller_environment, fallback_file, c_error_dir, retry_args, is_verbose, input_path] () {
		retry_macos_v3_with_old_compiler(caller_environment, fallback_file, c_error_dir,
			retry_args, is_verbose, input_path)
	}) or {
		eprintln('cannot register the V3 compatibility fallback: ${err}')
		exit(1)
	}
	macos_v3_driver_run(forwarded_args)
	os.rm(fallback_file) or {}
	os.rmdir_all(c_error_dir) or {}
	exit(0)
}

fn replace_macos_v3_process_environment(environment map[string]string) {
	current := os.environ()
	for name, _ in current {
		if name !in environment {
			os.unsetenv(name)
		}
	}
	for name, value in environment {
		os.setenv(name, value, true)
	}
}

fn retry_macos_v3_with_old_compiler(caller_environment map[string]string, fallback_file string, c_error_dir string, retry_args []string, is_verbose bool, input_path string) {
	fallback_reason := os.read_file(fallback_file) or { return }
	os.rm(fallback_file) or {}
	if fallback_reason !in [macos_v3_inline_asm_fallback, macos_v3_compiler_error_fallback,
		macos_v3_c_error_fallback] {
		os.rmdir_all(c_error_dir) or {}
		return
	}
	if os.getenv(macos_v3_no_fallback_env) == '1' {
		if fallback_reason == macos_v3_c_error_fallback {
			if report := read_macos_v3_c_error_report(c_error_dir) {
				if report.c_output != '' {
					eprintln(report.c_output.trim_right('\r\n'))
				}
			}
		}
		os.rmdir_all(c_error_dir) or {}
		eprintln('V3 compatibility fallback disabled; requested reason: ${fallback_reason}')
		return
	}
	replace_macos_v3_process_environment(caller_environment)
	should_report := is_verbose || os.getenv('V3_CACHE_TRACE') != ''
	if fallback_reason == macos_v3_c_error_fallback {
		read_macos_v3_c_error_report(c_error_dir) or {
			os.rmdir_all(c_error_dir) or {}
			eprintln('V3 requested a C-error fallback, but its diagnostics could not be read')
			return
		}
		// The C-error report was staged by the V3 driver (generated C + C compiler
		// output). Hand it to the V1 retry, which files a bug once its build
		// succeeds (see cmd/v rebuild -> compile_with_external_c_error_report).
		os.setenv(macos_v3_c_error_dir_env, c_error_dir, true)
		if should_report {
			eprintln('V3 C compilation failed; retrying with `-old-compiler`.')
		}
	} else if fallback_reason == macos_v3_compiler_error_fallback {
		// V3 hit an internal compiler error (parser/checker/codegen) that V1 may
		// still handle. Stage a report so the V1 retry, once its build succeeds,
		// prints the fallback notice and (for a single-file build) files a bug with
		// the input V source. Directory builds (`v .`) stage a notice-only report so
		// the fallback is never silent.
		if stage_macos_v3_compiler_error_report(c_error_dir, input_path) {
			os.setenv(macos_v3_c_error_dir_env, c_error_dir, true)
		} else {
			os.rmdir_all(c_error_dir) or {}
		}
		if should_report {
			eprintln('V3 compilation failed; retrying with `-old-compiler`.')
		}
	} else {
		// Inline assembly is a known, expected V3 limitation, not a bug: fall back
		// quietly without filing a report.
		os.rmdir_all(c_error_dir) or {}
		if should_report {
			println('V3 requested the compatibility compiler for inline assembly')
		}
	}
	os.setenv(macos_v3_retry_env, '1', true)
	executable := os.executable()
	os.execvp(executable, retry_args) or {
		os.rmdir_all(c_error_dir) or {}
		eprintln('failed to launch the compatibility compiler `${executable}`: ${err}')
	}
}

// stage_macos_v3_compiler_error_report writes a report directory describing a V3
// internal compiler error, mirroring the layout the V3 driver uses for C errors
// (see request_macos_v3_c_error_fallback). When the build targets a single V
// source file, that file is copied in as a reproducer. For a directory build
// (`v .`) or a non-V input no single file can be staged, so the report is
// notice-only (empty source name): the V1 retry can still tell the user V3 fell
// back once its own build succeeds, it just cannot upload a source snippet. It
// returns false only when the report directory itself cannot be created.
fn stage_macos_v3_compiler_error_report(report_dir string, input_path string) bool {
	if report_dir == '' {
		return false
	}
	os.rmdir_all(report_dir) or {}
	os.mkdir_all(report_dir) or { return false }
	mut source_name := ''
	if input_path != '' && os.is_file(input_path)
		&& (input_path.ends_with('.v') || input_path.ends_with('.vsh')
		|| input_path.ends_with('.vv')) {
		candidate := os.base(input_path)
		os.cp(input_path, os.join_path(report_dir, candidate)) or {}
		if os.is_file(os.join_path(report_dir, candidate)) {
			source_name = candidate
		}
	}
	staged := {
		macos_v3_c_error_source_name_file: source_name
		macos_v3_c_error_compiler_file:    'v3'
		macos_v3_c_error_output_file:      'error: the experimental V3 compiler hit an internal compiler error building this program (the stable V compiler built it successfully)'
		macos_v3_c_error_kind_file:        macos_v3_compiler_error_fallback
	}
	for name, value in staged {
		os.write_file(os.join_path(report_dir, name), value) or {
			os.rmdir_all(report_dir) or {}
			return false
		}
	}
	return true
}

fn take_macos_v3_c_error_report() ?MacosV3CErrorReport {
	report_dir := os.getenv(macos_v3_c_error_dir_env)
	if report_dir == '' {
		return none
	}
	os.unsetenv(macos_v3_c_error_dir_env)
	return read_macos_v3_c_error_report(report_dir)
}

fn macos_v3_c_error_report_dir(fallback_file string) string {
	return fallback_file + '.c_error'
}

fn read_macos_v3_c_error_report(report_dir string) ?MacosV3CErrorReport {
	ccompiler := os.read_file(os.join_path(report_dir, macos_v3_c_error_compiler_file)) or {
		return none
	}
	c_output := os.read_file(os.join_path(report_dir, macos_v3_c_error_output_file)) or {
		return none
	}
	// The `kind` marker is absent for C-error reports staged by the V3 driver
	// (empty string -> generated-C compilation error) and set for compiler-error
	// reports staged by stage_macos_v3_compiler_error_report.
	kind :=
		(os.read_file(os.join_path(report_dir, macos_v3_c_error_kind_file)) or { '' }).trim_space()
	clean_source_name := (os.read_file(os.join_path(report_dir, macos_v3_c_error_source_name_file)) or {
		''
	}).trim_space()
	mut c_file := ''
	if clean_source_name != '' {
		if os.base(clean_source_name) != clean_source_name {
			return none
		}
		candidate := os.join_path(report_dir, clean_source_name)
		if !os.is_file(candidate) {
			return none
		}
		c_file = candidate
	} else if kind != macos_v3_compiler_error_fallback {
		// A generated-C error report must always carry its source; only a
		// compiler-error report may be notice-only (a directory / non-file build).
		return none
	}
	return MacosV3CErrorReport{
		kind:       kind
		ccompiler:  ccompiler.trim_space()
		c_output:   c_output
		c_file:     c_file
		report_dir: report_dir
	}
}

fn macos_v3_child_environment(vexe string, fallback_file string, caller_environment map[string]string) map[string]string {
	mut environment := caller_environment.clone()
	preserve_macos_v3_caller_environment_value(mut environment, caller_environment, 'VEXE',
		macos_v3_caller_vexe_env, macos_v3_caller_vexe_present_env)
	preserve_macos_v3_caller_environment_value(mut environment, caller_environment, 'VCHILD',
		macos_v3_caller_vchild_env, macos_v3_caller_vchild_present_env)
	for private_name in ['V_MACOS_V3_FALLBACK_FILE', 'V_MACOS_V3_C_ERROR_DIR', 'V_MACOS_V3_RETRY'] {
		environment.delete(private_name)
	}
	environment['VCHILD'] = 'true'
	environment['VEXE'] = os.real_path(vexe)
	environment[macos_v3_fallback_file_env] = fallback_file
	environment[macos_v3_c_error_dir_env] = macos_v3_c_error_report_dir(fallback_file)
	environment[macos_v3_vhash_env] = @VHASH
	environment[macos_v3_vcurrent_hash_env] = @VCURRENTHASH
	environment[macos_v3_embedded_env] = '1'
	return environment
}

fn preserve_macos_v3_caller_environment_value(mut environment map[string]string, caller_environment map[string]string, name string, value_name string, present_name string) {
	if value := caller_environment[name] {
		environment[value_name] = value
		environment[present_name] = '1'
	} else {
		environment[value_name] = ''
		environment[present_name] = '0'
	}
}
