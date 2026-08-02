module main

import os
import v.pref
import v.util

const macos_v3_fallback_file_env = 'V_MACOS_V3_FALLBACK_FILE'
const macos_v3_c_error_dir_env = 'V_MACOS_V3_C_ERROR_DIR'
const macos_v3_vhash_env = 'V_MACOS_V3_VHASH'
const macos_v3_vcurrent_hash_env = 'V_MACOS_V3_VCURRENT_HASH'
const macos_v3_compat_c99_flag = '-macos-v3-compat-c99'
const macos_v3_internal_quiet_flag = '-macos-v3-internal-quiet'
const macos_v3_caller_vexe_env = 'V_MACOS_V3_CALLER_VEXE'
const macos_v3_caller_vexe_present_env = 'V_MACOS_V3_CALLER_VEXE_PRESENT'
const macos_v3_caller_vchild_env = 'V_MACOS_V3_CALLER_VCHILD'
const macos_v3_caller_vchild_present_env = 'V_MACOS_V3_CALLER_VCHILD_PRESENT'
const macos_v3_embedded_env = 'V_MACOS_V3_EMBEDDED'
const macos_v3_retry_env = 'V_MACOS_V3_RETRY'
const macos_v3_inline_asm_fallback = 'inline_asm'
const macos_v3_compiler_error_fallback = 'compiler_error'
const macos_v3_c_error_fallback = 'c_compilation_error'
const macos_v3_c_error_compiler_file = 'compiler'
const macos_v3_c_error_output_file = 'output'
const macos_v3_c_error_source_name_file = 'source_name'

fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) ?MacosV3CErrorReport {
	if os.getenv(macos_v3_retry_env) == '1' {
		os.unsetenv(macos_v3_retry_env)
		return take_macos_v3_c_error_report()
	}
	if prefs.old_compiler {
		return take_macos_v3_c_error_report()
	}
	if !macos_v3_driver_is_available() {
		return take_macos_v3_c_error_report()
	}
	all_args := util.join_env_vflags_and_os_args()
	forwarded_args := all_args[1..]
	if !is_macos_v3_default_executable(os.executable()) {
		trace_macos_v3_skip('non-default compiler executable `${os.executable()}`')
		return none
	}
	if macos_v3_has_v1_only_leading_option(forwarded_args, command) {
		return none
	}
	if !is_macos_v3_relevant_command(command, prefs) {
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

fn macos_v3_has_v1_only_leading_option(args []string, command string) bool {
	for arg in args {
		if arg == '--' || (command.len > 0 && arg == command) {
			return false
		}
		if arg == '-message-limit' {
			return true
		}
	}
	return false
}

fn is_macos_v3_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.old_compiler {
		return false
	}
	if prefs.sanitize || prefs.is_livemain || prefs.is_liveshared || prefs.is_prof
		|| prefs.output_cross_c || prefs.experimental || prefs.is_apk
		|| prefs.json_errors || prefs.no_preludes || prefs.skip_warnings
		|| prefs.print_watched_files || prefs.is_vlines || prefs.warn_impure_v
		|| prefs.test_runner.len > 0 || prefs.exclude.len > 0
		|| prefs.ldflags.len > 0 || prefs.nofloat || prefs.fast_math
		|| prefs.compress || prefs.is_bare || prefs.assert_failure_mode != .default
		|| prefs.build_options.any(it in ['-m32', '-m64']) || prefs.backend.is_js()
		|| (prefs.backend == .wasm && prefs.is_run) || prefs.gc_mode != .no_gc {
		// V1 still owns compiler modes whose runtime or C toolchain support has not
		// been implemented by V3 yet.
		return false
	}
	if prefs.autofree && prefs.is_run {
		// V1 still owns the established `v -autofree run ...` orchestration.
		// Direct autofree builds are selected earlier by the ownership dispatcher.
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
	if command in ['help', 'version', 'new', 'init', 'install', 'link', 'list', 'outdated', 'remove',
		'search', 'show', 'unlink', 'update', 'upgrade', 'vlib-docs', 'interpret', 'get', 'translate',
		'crun'] {
		return false
	}
	return command in ['run', 'build', 'test'] || prefs.is_script || os.is_dir(prefs.path)
		|| normalized_path.ends_with('.v') || normalized_path.ends_with('.vv')
		|| normalized_path.ends_with('.vsh')
}

fn is_macos_v3_internal_tool_bootstrap(normalized_path string, is_vchild bool) bool {
	return is_vchild
		&& (normalized_path.starts_with('cmd/tools/') || normalized_path.contains('/cmd/tools/'))
}

fn macos_v3_forwarded_args(prefs &pref.Preferences, raw_args []string) []string {
	mut forwarded_args := raw_args.clone()
	// V1 treats `x86` as an amd64 alias, while V3 reserves it for the 32-bit target.
	if prefs.arch == .amd64 && '-arch x86' in prefs.build_options {
		for i in 0 .. forwarded_args.len {
			if i + 1 < forwarded_args.len && forwarded_args[i] == '-arch'
				&& forwarded_args[i + 1] == 'x86' {
				forwarded_args[i + 1] = 'amd64'
				break
			}
		}
	}
	if macos_v3_compat_c99_flag !in forwarded_args {
		forwarded_args.insert(0, macos_v3_compat_c99_flag)
	}
	if prefs.skip_running && '-skip-running' !in forwarded_args {
		forwarded_args.insert(0, '-skip-running')
	}
	if !prefs.is_verbose && !prefs.is_stats && !prefs.show_timings && '-silent' !in forwarded_args
		&& macos_v3_internal_quiet_flag !in forwarded_args {
		forwarded_args.insert(0, macos_v3_internal_quiet_flag)
	}
	// The compatibility fallback must not select a different compiler merely
	// because a valid V3 build crosses the standalone driver's safety cap.
	if '-no-memory-limit' !in forwarded_args && '--no-memory-limit' !in forwarded_args {
		forwarded_args.insert(0, '-no-memory-limit')
	}
	return forwarded_args
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
	environment := macos_v3_child_environment(vexe, fallback_file, caller_environment)
	replace_macos_v3_process_environment(environment)
	is_verbose := prefs.is_verbose
	retry_args := os.args[1..].clone()
	at_exit(fn [caller_environment, fallback_file, c_error_dir, retry_args, is_verbose] () {
		retry_macos_v3_with_old_compiler(caller_environment, fallback_file, c_error_dir,
			retry_args, is_verbose)
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

fn retry_macos_v3_with_old_compiler(caller_environment map[string]string, fallback_file string, c_error_dir string, retry_args []string, is_verbose bool) {
	fallback_reason := os.read_file(fallback_file) or { return }
	os.rm(fallback_file) or {}
	if fallback_reason !in [macos_v3_inline_asm_fallback, macos_v3_compiler_error_fallback,
		macos_v3_c_error_fallback] {
		os.rmdir_all(c_error_dir) or {}
		return
	}
	replace_macos_v3_process_environment(caller_environment)
	should_report := is_verbose || os.getenv('V3_CACHE_TRACE') != ''
	if fallback_reason == macos_v3_c_error_fallback {
		report := read_macos_v3_c_error_report(c_error_dir) or {
			os.rmdir_all(c_error_dir) or {}
			eprintln('V3 requested a C-error fallback, but its diagnostics could not be read')
			return
		}
		os.setenv(macos_v3_c_error_dir_env, c_error_dir, true)
		if should_report {
			eprintln('V3 C compilation failed; retrying with `-old-compiler`.')
			if report.c_output != '' {
				eprintln(report.c_output.trim_right('\r\n'))
			}
		}
	} else {
		os.rmdir_all(c_error_dir) or {}
		if fallback_reason == macos_v3_inline_asm_fallback {
			if should_report {
				println('V3 requested the compatibility compiler for inline assembly')
			}
		} else {
			if should_report {
				eprintln('V3 compilation failed; retrying with `-old-compiler`.')
			}
		}
	}
	os.setenv(macos_v3_retry_env, '1', true)
	executable := os.executable()
	os.execvp(executable, retry_args) or {
		os.rmdir_all(c_error_dir) or {}
		eprintln('failed to launch the compatibility compiler `${executable}`: ${err}')
	}
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
	source_name := os.read_file(os.join_path(report_dir, macos_v3_c_error_source_name_file)) or {
		return none
	}
	clean_source_name := source_name.trim_space()
	if clean_source_name == '' || os.base(clean_source_name) != clean_source_name {
		return none
	}
	ccompiler := os.read_file(os.join_path(report_dir, macos_v3_c_error_compiler_file)) or {
		return none
	}
	c_output := os.read_file(os.join_path(report_dir, macos_v3_c_error_output_file)) or {
		return none
	}
	c_file := os.join_path(report_dir, clean_source_name)
	if !os.is_file(c_file) {
		return none
	}
	return MacosV3CErrorReport{
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
