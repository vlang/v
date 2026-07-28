module main

import hash
import os
import os.filelock
import time
import v.pref
import v.util

const macos_v3_bootstrap_env = 'V_MACOS_V3_BOOTSTRAP'
const macos_v3_executable_env = 'V_MACOS_V3_EXECUTABLE'

fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) {
	all_args := util.join_env_vflags_and_os_args()
	forwarded_args := all_args[1..]
	if os.getenv(macos_v3_bootstrap_env) != '' || !is_macos_v3_default_executable(os.executable())
		|| !is_macos_v3_relevant_command(command, prefs)
		|| !macos_v3_args_are_supported(forwarded_args) {
		return
	}
	launch_macos_v3_compiler(prefs, forwarded_args)
}

fn is_macos_v3_default_executable(vexe string) bool {
	return os.base(vexe) in ['v', 'v.exe', 'vnew', 'vnew.exe']
}

fn is_macos_v3_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.path == '' || prefs.backend != .c || prefs.os != .macos {
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
		|| prefs.exclude.len > 0 {
		return false
	}
	if prefs.gc_mode != .no_gc {
		return false
	}
	if normalized_path == 'cmd/v' || normalized_path.ends_with('/cmd/v')
		|| normalized_path.ends_with('/cmd/v/v.v') || normalized_path.starts_with('cmd/tools/')
		|| normalized_path.contains('/cmd/tools/') || normalized_path.ends_with('.vv')
		|| os.is_dir(prefs.path) || macos_v3_source_path_resolves_differently(prefs.path)
		|| macos_v3_needs_compatible_default_output(prefs.path) {
		// Keep the established compiler available as the compatibility fallback.
		// V3 does not compile all of cmd/v yet, and command tools are built on
		// demand while dispatching CLI commands such as `fmt` and `test`. Legacy
		// .vv fixtures, directory builds, and sources needing compatibility
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
		if arg in ['-o', '-b', '-os', '-arch', '-compile-backend', '--compile-backend', '-d', '-gc',
			'-cc', '-cflags'] {
			if i + 1 >= args.len {
				return false
			}
			i += 2
			continue
		}
		if arg.starts_with('-d') && arg.len > 2 {
			i++
			continue
		}
		if arg in ['-prod', '-shared', '--shared', '-selfhost', '-building-v', '-building_v', '-c99',
			'--c99', '-strict', '-cstrict', '-ownership', '--ownership', '-no-parallel',
			'--no-parallel', '-parallel-transform', '--parallel-transform', '-all-backends',
			'--all-backends', '-g', '-cg', '-autofree', '-v', '-silent', '-checker-fixture', '-stats',
			'-show-timings', '-showcc', '-keepc', '-w', '-no-retry-compilation', '-skip-running',
			'-usecache', '-no-prealloc', '--no-prealloc', '-nocache', '--no-cache',
			'-no-memory-limit', '--no-memory-limit', '-prealloc', '-enable-globals'] {
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
	if prefs.skip_running && '-skip-running' !in forwarded_args {
		forwarded_args.insert(0, '-skip-running')
	}
	if !prefs.is_verbose && !prefs.is_stats && !prefs.show_timings && !prefs.show_cc {
		forwarded_args.insert(0, '-silent')
	}
	// Serial stages keep cold-cache builds of larger programs below V3's
	// physical-memory safety limit.
	if '-no-parallel' !in forwarded_args {
		forwarded_args.insert(0, '-no-parallel')
	}
	return forwarded_args
}

@[noreturn]
fn launch_macos_v3_compiler(prefs &pref.Preferences, raw_args []string) {
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
	mut environment := os.environ()
	environment['VCHILD'] = 'true'
	environment['VEXE'] = os.real_path(vexe)
	process.set_environment(environment)
	process.run()
	process.wait()
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	exit(exit_code)
}

fn build_macos_v3_compiler(vexe string, vroot string, v3_source string, v3_exe string, is_verbose bool) {
	args := ['-prealloc', '-o', v3_exe, v3_source]
	if is_verbose {
		println('Compiling macOS V3 compiler with: ${os.quoted_path(vexe)} ${util.args_quote_paths(args)}')
	}
	mut process := os.new_process(vexe)
	process.set_work_folder(vroot)
	process.set_args(args)
	mut environment := os.environ()
	environment[macos_v3_bootstrap_env] = '1'
	environment['VFLAGS'] = ''
	process.set_environment(environment)
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

fn cached_macos_v3_executable_path(vroot string) string {
	vroot_hash := hash.sum64_string(os.real_path(vroot), 0).hex_full()
	return util.path_of_executable(os.join_path(os.vtmp_dir(), 'v', 'delegated_v3', vroot_hash,
		'v3'))
}
