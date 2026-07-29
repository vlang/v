module main

import hash
import os
import os.filelock
import time
import v.pref
import v.util

const macos_v3_bootstrap_env = 'V_MACOS_V3_BOOTSTRAP'
const macos_v3_executable_env = 'V_MACOS_V3_EXECUTABLE'
const macos_v3_fallback_file_env = 'V_MACOS_V3_FALLBACK_FILE'
const macos_v3_c_error_dir_env = 'V_MACOS_V3_C_ERROR_DIR'
const macos_v3_vhash_env = 'V_MACOS_V3_VHASH'
const macos_v3_vcurrent_hash_env = 'V_MACOS_V3_VCURRENT_HASH'
const macos_v3_compat_c99_flag = '-macos-v3-compat-c99'
const macos_v3_caller_vexe_env = 'V_MACOS_V3_CALLER_VEXE'
const macos_v3_caller_vexe_present_env = 'V_MACOS_V3_CALLER_VEXE_PRESENT'
const macos_v3_caller_vchild_env = 'V_MACOS_V3_CALLER_VCHILD'
const macos_v3_caller_vchild_present_env = 'V_MACOS_V3_CALLER_VCHILD_PRESENT'
const macos_v3_inline_asm_fallback = 'inline_asm'
const macos_v3_compiler_error_fallback = 'compiler_error'
const macos_v3_c_error_fallback = 'c_compilation_error'
const macos_v3_c_error_compiler_file = 'compiler'
const macos_v3_c_error_output_file = 'output'
const macos_v3_c_error_source_name_file = 'source_name'
const macos_v3_enabled_product_version_major = 26
const macos_v3_enabled_product_version_minor = 2

fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) ?MacosV3CErrorReport {
	all_args := util.join_env_vflags_and_os_args()
	forwarded_args := all_args[1..]
	if os.getenv(macos_v3_bootstrap_env) != '' || !is_macos_v3_default_executable(os.executable())
		|| !is_macos_v3_relevant_command(command, prefs) || !macos_v3_default_is_enabled_for_host()
		|| !macos_v3_environment_flags_are_supported(os.getenv('CFLAGS'), os.getenv('LDFLAGS'))
		|| !macos_v3_args_are_supported(forwarded_args) {
		return none
	}
	return launch_macos_v3_compiler(prefs, forwarded_args)
}

fn macos_v3_default_is_enabled_for_host() bool {
	if os.getenv('GITHUB_ACTIONS') == 'true' {
		return true
	}
	version := os.execute('/usr/bin/sw_vers -productVersion')
	if version.exit_code != 0 {
		return false
	}
	return macos_v3_default_is_enabled(version.output, '')
}

fn macos_v3_default_is_enabled(product_version string, github_actions string) bool {
	if github_actions == 'true' {
		return true
	}
	parts := product_version.trim_space().split('.')
	if parts.len < 2 || !parts[0].is_int() || !parts[1].is_int() {
		return false
	}
	return parts[0].int() == macos_v3_enabled_product_version_major
		&& parts[1].int() == macos_v3_enabled_product_version_minor
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
	is_direct_vsh := normalized_path.ends_with('.vsh') && command != 'crun'
	if command in external_tools
		|| command in ['help', 'version', 'new', 'init', 'install', 'link', 'list', 'outdated', 'remove', 'search', 'show', 'unlink', 'update', 'upgrade', 'vlib-docs', 'interpret', 'get', 'translate'] {
		return false
	}
	if (prefs.is_crun && !is_direct_vsh) || prefs.is_test || prefs.is_prod
		|| prefs.autofree || prefs.build_mode == .build_module || prefs.is_cstrict
		|| prefs.use_cache || prefs.parallel_cc || prefs.out_name_is_dir
		|| prefs.exclude.len > 0 || prefs.coverage_dir != '' || prefs.is_o
		|| prefs.is_vlines || prefs.is_shared {
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
		|| (!normalized_path.ends_with('.v') && !normalized_path.ends_with('.vsh'))
		|| os.is_dir(prefs.path)
		|| macos_v3_source_path_resolves_differently(prefs.path)
		|| macos_v3_needs_compatible_default_output(prefs.path) {
		// Keep the established compiler available as the compatibility fallback.
		// V3 does not compile all of cmd/v yet, and command tools are built on
		// demand while dispatching CLI commands such as `fmt` and `test`. Legacy
		// .vv fixtures, directory/non-V builds, and sources needing compatibility
		// output-name derivation also retain established semantics.
		return false
	}
	return command == 'run' || command == 'build' || prefs.is_script
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
	// Serial stages keep cold-cache builds of larger programs below V3's
	// physical-memory safety limit.
	if '-no-parallel' !in forwarded_args {
		forwarded_args.insert(0, '-no-parallel')
	}
	return forwarded_args
}

fn launch_macos_v3_compiler(prefs &pref.Preferences, raw_args []string) ?MacosV3CErrorReport {
	caller_environment := os.environ()
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	util.set_vroot_folder(vroot)
	v3_source := os.join_path(vroot, 'vlib', 'v3', 'v3.v')
	v3_source_dir := os.dir(v3_source)
	mut v3_exe := os.getenv(macos_v3_executable_env)
	if v3_exe == '' {
		v3_exe = cached_macos_v3_executable_path(vroot)
		os.mkdir_all(os.dir(v3_exe)) or {
			eprintln('cannot create `${os.dir(v3_exe)}`: ${err}')
			exit(1)
		}
		mut build_lock := filelock.new(v3_exe + '.lock')
		if !build_lock.wait_acquire(10 * time.minute) {
			eprintln('timed out waiting to build `${v3_source}`')
			exit(1)
		}
		if util.should_recompile_tool(vexe, v3_source_dir, 'v3', v3_exe) {
			build_macos_v3_compiler(vexe, vroot, v3_source, v3_exe, prefs.is_verbose)
		}
		build_lock.release()
	}
	forwarded_args := macos_v3_forwarded_args(prefs, raw_args)
	if prefs.is_verbose {
		println('Launching macOS V3 compiler: ${os.quoted_path(v3_exe)} ${util.args_quote_paths(forwarded_args)}')
	}
	mut process := os.new_process(v3_exe)
	process.set_args(forwarded_args)
	fallback_file := os.join_path(os.vtmp_dir(), 'macos_v3_fallback_${os.getpid()}')
	os.rm(fallback_file) or {}
	c_error_dir := macos_v3_c_error_report_dir(fallback_file)
	os.rmdir_all(c_error_dir) or {}
	environment := macos_v3_child_environment(vexe, fallback_file, caller_environment)
	process.set_environment(environment)
	process.run()
	process.wait()
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	fallback_reason := os.read_file(fallback_file) or { '' }
	os.rm(fallback_file) or {}
	if exit_code != 0 && fallback_reason == macos_v3_inline_asm_fallback {
		os.rmdir_all(c_error_dir) or {}
		if prefs.is_verbose {
			println('V3 requested the compatibility compiler for inline assembly')
		}
		return none
	}
	if exit_code != 0 && fallback_reason == macos_v3_c_error_fallback {
		report := read_macos_v3_c_error_report(c_error_dir) or {
			os.rmdir_all(c_error_dir) or {}
			eprintln('V3 requested a C-error fallback, but its diagnostics could not be read')
			exit(exit_code)
		}
		eprintln('V3 C compilation failed; retrying with `-old-compiler`.')
		if report.c_output != '' {
			eprintln(report.c_output.trim_right('\r\n'))
		}
		return report
	}
	if exit_code != 0 && fallback_reason == macos_v3_compiler_error_fallback {
		os.rmdir_all(c_error_dir) or {}
		eprintln('V3 compilation failed; retrying with `-old-compiler`.')
		return none
	}
	os.rmdir_all(c_error_dir) or {}
	exit(exit_code)
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

fn build_macos_v3_compiler(vexe string, vroot string, v3_source string, v3_exe string, is_verbose bool) {
	args := ['-prealloc', '-o', v3_exe, v3_source]
	if is_verbose {
		println('Compiling macOS V3 compiler with: ${os.quoted_path(vexe)} ${util.args_quote_paths(args)}')
	}
	mut process := os.new_process(vexe)
	process.set_work_folder(vroot)
	process.set_args(args)
	process.set_environment(macos_v3_bootstrap_environment())
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	if exit_code != 0 {
		eprintln('cannot compile `${v3_source}`: ${exit_code}\n${output}')
		exit(1)
	}
	if is_verbose && output != '' {
		print(output)
	}
}

fn macos_v3_bootstrap_environment() map[string]string {
	mut environment := os.environ()
	environment[macos_v3_bootstrap_env] = '1'
	environment['VFLAGS'] = ''
	environment['VOSARGS'] = ''
	return environment
}

fn cached_macos_v3_executable_path(vroot string) string {
	vroot_hash := hash.sum64_string(os.real_path(vroot), 0).hex_full()
	return util.path_of_executable(os.join_path(os.vtmp_dir(), 'v', 'delegated_v3', vroot_hash,
		'v3'))
}
