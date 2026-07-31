module main

import os
import v.pref
import v.util

const macos_v3_fallback_file_env = 'V_MACOS_V3_FALLBACK_FILE'
const macos_v3_c_error_dir_env = 'V_MACOS_V3_C_ERROR_DIR'
const macos_v3_vhash_env = 'V_MACOS_V3_VHASH'
const macos_v3_vcurrent_hash_env = 'V_MACOS_V3_VCURRENT_HASH'
const macos_v3_compat_c99_flag = '-macos-v3-compat-c99'
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
	if !is_macos_v3_default_executable(os.executable())
		|| !is_macos_v3_relevant_command(command, prefs)
		|| !macos_v3_environment_flags_are_supported(os.getenv('CFLAGS'), os.getenv('LDFLAGS'))
		|| !macos_v3_args_are_supported(forwarded_args) {
		return none
	}
	return launch_macos_v3_compiler(prefs, forwarded_args)
}

fn macos_v3_environment_flags_are_supported(cflags string, ldflags string) bool {
	return cflags == '' && ldflags == ''
}

fn is_macos_v3_default_executable(vexe string) bool {
	return os.base(vexe) in ['v', 'v.exe', 'vnew', 'vnew.exe']
}

fn is_macos_v3_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.old_compiler || prefs.path == '' || prefs.backend != .c || prefs.os != .macos {
		return false
	}
	normalized_path := prefs.path.replace('\\', '/').trim_right('/')
	is_directory := os.is_dir(prefs.path)
	if command in external_tools
		|| command in ['help', 'version', 'new', 'init', 'install', 'link', 'list', 'outdated', 'remove', 'search', 'show', 'unlink', 'update', 'upgrade', 'vlib-docs', 'interpret', 'get', 'translate'] {
		return false
	}
	if prefs.output_cross_c || prefs.is_crun || prefs.is_test || prefs.is_prod || prefs.autofree
		|| prefs.build_mode == .build_module || prefs.is_cstrict || prefs.use_cache
		|| prefs.parallel_cc || prefs.out_name_is_dir || prefs.exclude.len > 0
		|| prefs.coverage_dir != '' || prefs.is_o || prefs.is_vlines || prefs.is_shared {
		return false
	}
	// The established preference defaults select Boehm before dispatch runs,
	// while V3 currently supports only no-GC builds. Treat that implicit mode
	// as part of the V3 default; preserve explicit non-none `-gc` selections by
	// keeping them on the compatibility compiler.
	if prefs.gc_set_by_flag && prefs.gc_mode != .no_gc {
		return false
	}
	if normalized_path == 'cmd/v' || normalized_path.ends_with('/cmd/v')
		|| normalized_path.ends_with('/cmd/v/v.v')
		|| normalized_path.starts_with('cmd/tools/')
		|| normalized_path.contains('/cmd/tools/')
		|| normalized_path.ends_with('.vv')
		|| (!is_directory && !normalized_path.ends_with('.v') && !normalized_path.ends_with('.vsh'))
		|| macos_v3_source_path_resolves_differently(prefs.path)
		|| macos_v3_needs_compatible_default_output(prefs.path) {
		// Keep the established compiler available as the compatibility fallback.
		// V3 does not compile all of cmd/v yet, and command tools are built on
		// demand while dispatching CLI commands such as `fmt` and `test`. Legacy
		// .vv fixtures, non-V builds, and sources needing compatibility output-name
		// derivation also retain established semantics.
		return false
	}
	return is_directory || command == 'run' || command == 'build' || prefs.is_script
		|| normalized_path.ends_with('.v') || normalized_path.ends_with('.vsh')
}

fn macos_v3_source_path_resolves_differently(path string) bool {
	if !os.exists(path) {
		return false
	}
	return os.norm_path(os.real_path(path)) != os.norm_path(os.abs_path(path))
}

fn macos_v3_args_are_supported(args []string) bool {
	mut input_seen := false
	mut should_run := false
	mut i := 0
	for i < args.len {
		arg := args[i]
		if should_run && input_seen {
			// Everything after the input to `run`, or after a direct V script,
			// belongs to the program rather than the compiler.
			i++
			continue
		}
		if arg == 'run' && !input_seen {
			should_run = true
			i++
			continue
		}
		if arg in ['build', 'test'] && !input_seen {
			i++
			continue
		}
		if arg == '-d' {
			if i + 1 >= args.len || args[i + 1].contains('=') {
				return false
			}
			i += 2
			continue
		}
		if arg in ['-o', '-b', '-os', '-arch', '-compile-backend', '--compile-backend', '-gc',
			'-cflags'] {
			if i + 1 >= args.len {
				return false
			}
			if arg == '-o' && args[i + 1].starts_with('-') {
				return false
			}
			if arg == '-arch' && !macos_v3_arch_is_supported(args[i + 1]) {
				return false
			}
			i += 2
			continue
		}
		if arg in ['-debug', '-debug-tcc', '-define', '-disable-explicit-mutability',
			'-div-by-zero-is-zero', '-dump-c-flags', '-dump-modules', '-dump-files', '-dump-defines'] {
			return false
		}
		if arg.starts_with('-d') && arg.len > 2 {
			if arg.contains('=') {
				return false
			}
			i++
			continue
		}
		if arg in ['-prod', '-shared', '--shared', '-selfhost', '-building-v', '-building_v', '-c99',
			'--c99', '-strict', '-cstrict', '-ownership', '--ownership', '-no-parallel',
			'--no-parallel', '-parallel-transform', '--parallel-transform', '-all-backends',
			'--all-backends', '-cg', '-autofree', '-v', '-checker-fixture', '-stats', '-show-timings',
			'-showcc', '-keepc', '-skip-running', '-usecache', '-no-prealloc', '--no-prealloc',
			'-nocache', '--no-cache', '-no-memory-limit', '--no-memory-limit', '-prealloc',
			'-enable-globals'] {
			i++
			continue
		}
		if arg.starts_with('-') || input_seen {
			return false
		}
		input_seen = true
		if arg.ends_with('.vsh') {
			should_run = true
		}
		i++
	}
	return input_seen
}

fn macos_v3_arch_is_supported(arch_name string) bool {
	// The established CLI treats this legacy alias as amd64, while V3 uses
	// x86 for its 32-bit target.
	if arch_name == 'x86' {
		return false
	}
	arch := pref.arch_from_string(arch_name) or { return false }
	return arch in [.amd64, .arm64, .arm32, .rv64, .i386, .s390x, .ppc64le, .loongarch64, .ppc64,
		.wasm32]
}

fn macos_v3_needs_compatible_default_output(path string) bool {
	raw_filename := os.file_name(path)
	filename := raw_filename.trim_space()
	if filename != raw_filename {
		return true
	}
	base := filename.all_before_last('.')
	if base == '' || base in ['.', '..', '-'] || os.file_ext(base) in ['.c', '.js', '.wasm'] {
		return true
	}
	if base == filename && filename.starts_with('.') {
		return true
	}
	for c in base {
		if c < ` ` || c == 127 {
			return true
		}
	}
	return base.ends_with('.c') || base.ends_with('.js') || base.ends_with('.wasm')
}

fn macos_v3_forwarded_args(prefs &pref.Preferences, raw_args []string) []string {
	mut forwarded_args := raw_args.clone()
	if macos_v3_compat_c99_flag !in forwarded_args {
		forwarded_args.insert(0, macos_v3_compat_c99_flag)
	}
	if prefs.skip_running && '-skip-running' !in forwarded_args {
		forwarded_args.insert(0, '-skip-running')
	}
	if !prefs.is_verbose && !prefs.is_stats && !prefs.show_timings {
		forwarded_args.insert(0, '-silent')
	}
	// The compatibility fallback must not select a different compiler merely
	// because a valid V3 build crosses the standalone driver's safety cap.
	if '-no-memory-limit' !in forwarded_args && '--no-memory-limit' !in forwarded_args {
		forwarded_args.insert(0, '-no-memory-limit')
	}
	// An embedded V3 driver cannot restart itself by replacing the cmd/v process.
	// Keep its first in-process rollout monolithic until cache invalidation can
	// restart the driver through an ordinary function return.
	if '-nocache' !in forwarded_args && '--no-cache' !in forwarded_args {
		forwarded_args.insert(0, '-nocache')
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
	if fallback_reason == macos_v3_c_error_fallback {
		report := read_macos_v3_c_error_report(c_error_dir) or {
			os.rmdir_all(c_error_dir) or {}
			eprintln('V3 requested a C-error fallback, but its diagnostics could not be read')
			return
		}
		os.setenv(macos_v3_c_error_dir_env, c_error_dir, true)
		if is_verbose {
			eprintln('V3 C compilation failed; retrying with `-old-compiler`.')
			if report.c_output != '' {
				eprintln(report.c_output.trim_right('\r\n'))
			}
		}
	} else {
		os.rmdir_all(c_error_dir) or {}
		if fallback_reason == macos_v3_inline_asm_fallback {
			if is_verbose {
				println('V3 requested the compatibility compiler for inline assembly')
			}
		} else {
			if is_verbose {
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
