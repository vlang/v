module main

// The V3 compiler is linked directly into `cmd/v` on macOS and Linux. The
// command shell hands C-backend compilations to it in-process while leaving
// tool commands and other backends on their existing external-tool paths.
// When V3 fails an ordinary program compilation, the lean V3-only command
// shell execs the separately built full V1 command binary instead of linking
// V1 back into the main `v` executable.
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
const macos_v3_caller_no_fallback_env = 'V_MACOS_V3_CALLER_NO_FALLBACK'
const macos_v3_caller_no_fallback_present_env = 'V_MACOS_V3_CALLER_NO_FALLBACK_PRESENT'
const macos_v3_embedded_env = 'V_MACOS_V3_EMBEDDED'
const macos_v3_retry_env = 'V_MACOS_V3_RETRY'
const macos_v3_no_fallback_env = 'V_MACOS_V3_NO_FALLBACK'
const macos_v3_inline_asm_fallback = 'inline_asm'
const macos_v3_compiler_error_fallback = 'compiler_error'
const macos_v3_c_error_fallback = 'c_compilation_error'
const macos_v3_v1_fallback_binary = 'v1_fallback'

struct MacosV3RetryState {
	caller_environment  map[string]string
	fallback_file       string
	c_error_dir         string
	retry_args          []string
	fallback_executable string
	is_verbose          bool
}

@[unsafe]
fn macos_v3_retry_state(state &MacosV3RetryState) &MacosV3RetryState {
	mut static retry_state := unsafe { &MacosV3RetryState(nil) }
	if state != unsafe { nil } {
		retry_state = state
	}
	return retry_state
}

fn retry_macos_v3_at_exit() {
	state := unsafe { macos_v3_retry_state(nil) }
	if state == unsafe { nil } {
		return
	}
	retry_macos_v3_with_v1(state)
}

fn retry_macos_v3_with_v1(state &MacosV3RetryState) {
	fallback_payload := os.read_file(state.fallback_file) or {
		os.rmdir_all(state.c_error_dir) or {}
		return
	}
	os.rm(state.fallback_file) or {}
	os.rmdir_all(state.c_error_dir) or {}
	fallback_reason := fallback_payload.all_before('\n').trim_space()
	if fallback_reason !in [macos_v3_inline_asm_fallback, macos_v3_compiler_error_fallback,
		macos_v3_c_error_fallback] {
		return
	}
	if os.getenv(macos_v3_no_fallback_env) == '1' {
		return
	}
	replace_macos_v3_process_environment(state.caller_environment)
	launch_macos_v1_fallback(state.fallback_executable, state.retry_args, state.is_verbose,
		'V3 compilation failed (${fallback_reason})')
}

fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) {
	$if macos || linux {
		if prefs.old_compiler {
			// `v self` and bootstrap passes build cmd/v with temporary names such as
			// v2/vstrict. Keep those compiler self-builds on V3 even if an inherited
			// VFLAGS still contains the historical selector. Everywhere else an
			// explicit -old-compiler means the separately built full V1 command shell.
			if macos_v3_is_self_build_target(prefs)
				&& os.base(os.executable()) !in ['v', 'v.exe', 'vnew', 'vnew.exe'] {
				all_args := util.join_env_vflags_and_os_args()
				bootstrap_args := all_args[1..].filter(it != '-old-compiler')
				launch_macos_v3_compiler(prefs, bootstrap_args)
			}
			fallback_executable := macos_v3_v1_fallback_executable()
			launch_macos_v1_fallback(fallback_executable, os.args[1..], prefs.is_verbose,
				'`-old-compiler` was requested')
		}
	}
	if !is_macos_v3_relevant_command(command, prefs) {
		return
	}
	if !macos_v3_driver_is_available() {
		if prefs.new_compiler {
			eprintln('`-new-compiler` requires a build that embeds the V3 compiler, which this one does not.')
			exit(1)
		}
		return
	}
	if message := macos_v3_fastc_incompatibility(prefs) {
		eprintln(message)
		exit(1)
	}
	if prefs.new_compiler && macos_v3_explicit_autofree_is_unsupported(prefs) {
		eprintln('`-new-compiler` cannot be combined with `-autofree`: the embedded V3 compiler does not include ownership support.')
		exit(1)
	}
	all_args := util.join_env_vflags_and_os_args()
	launch_macos_v3_compiler(prefs, all_args[1..])
}

@[noreturn]
fn launch_macos_v1_fallback(executable string, args []string, is_verbose bool, reason string) {
	if !os.is_executable(executable) {
		eprintln('${reason}, but the V1 compatibility compiler `${executable}` is missing. Rebuild V with `make` to create it.')
		exit(1)
	}
	// Keep nested tool/test compilations on V1 as well. Otherwise an inherited
	// caller VEXE can point back at the V3-only command binary and bypass the
	// compatibility compiler after the first retry.
	os.setenv('VEXE', os.real_path(executable), true)
	os.setenv('VCHILD', 'true', true)
	if is_verbose || os.getenv('V3_CACHE_TRACE') != '' {
		eprintln('${reason}; retrying with `${executable}`.')
	}
	os.execvp(executable, args) or {
		eprintln('failed to launch the V1 compatibility compiler `${executable}`: ${err}')
		exit(1)
	}
	exit(1)
}

fn macos_v3_v1_fallback_executable() string {
	return os.join_path(os.dir(pref.vexe_path()), macos_v3_v1_fallback_binary)
}

fn macos_v3_is_self_build_target(prefs &pref.Preferences) bool {
	if prefs.path == '' {
		return false
	}
	vroot := os.real_path(os.dir(pref.vexe_path())).replace('\\', '/').trim_right('/')
	target := os.real_path(prefs.path).replace('\\', '/').trim_right('/')
	return target == '${vroot}/cmd/v' || target == '${vroot}/cmd/v/v.v'
}

fn is_macos_v3_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.backend != .c || command == 'test' || command in external_tools
		|| macos_v3_non_compilation_command(command) || prefs.path == '' {
		return false
	}
	normalized_path := prefs.path.replace('\\', '/').trim_right('/')
	return command in ['run', 'build'] || prefs.is_script || os.is_dir(prefs.path)
		|| normalized_path.ends_with('.v') || normalized_path.ends_with('.vsh')
		|| normalized_path.ends_with('.vv')
}

@[noreturn]
fn launch_macos_v3_compiler(prefs &pref.Preferences, raw_args []string) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	util.set_vroot_folder(vroot)
	forwarded_args := macos_v3_forwarded_args(prefs, raw_args)
	if prefs.is_verbose {
		println('Running V3 compiler in process: ${util.args_quote_paths(forwarded_args)}')
	}
	dispatch_environment := os.environ()
	caller_environment := macos_v3_original_caller_environment(dispatch_environment)
	mut environment := macos_v3_child_environment(vexe, caller_environment, dispatch_environment)
	no_fallback := environment[macos_v3_no_fallback_env] or { '' }
	// cmd/v self-builds deliberately remain V3-only. The compatibility compiler
	// exists for user programs and tools, not as an alternate self-host path.
	fallback_enabled := !prefs.new_compiler && no_fallback != '1'
		&& !macos_v3_is_self_build_target(prefs)
	fallback_file := macos_v3_fallback_file_for_pid()
	c_error_dir := macos_v3_c_error_report_dir(fallback_file)
	os.rm(fallback_file) or {}
	os.rmdir_all(c_error_dir) or {}
	if fallback_enabled {
		environment[macos_v3_fallback_file_env] = fallback_file
		environment[macos_v3_c_error_dir_env] = c_error_dir
	} else if prefs.new_compiler {
		// An explicit `-new-compiler` remains strict even when the caller did not
		// provide the CI/debug no-fallback environment switch.
		environment[macos_v3_no_fallback_env] = '1'
	}
	replace_macos_v3_process_environment(environment)
	if fallback_enabled {
		retry_state := &MacosV3RetryState{
			caller_environment:  caller_environment
			fallback_file:       fallback_file
			c_error_dir:         c_error_dir
			retry_args:          os.args[1..].clone()
			fallback_executable: macos_v3_v1_fallback_executable()
			is_verbose:          prefs.is_verbose
		}
		unsafe { macos_v3_retry_state(retry_state) }
		at_exit(retry_macos_v3_at_exit) or {
			eprintln('cannot register the V3 compatibility fallback: ${err}')
			exit(1)
		}
		// Treat every early V3 exit while compiling an ordinary program as a
		// compatibility failure. V3 may overwrite this sentinel with a more
		// specific reason (inline asm or generated-C failure). A successful V3
		// compilation removes the file below before exiting.
		os.write_file(fallback_file, macos_v3_compiler_error_fallback) or {
			eprintln('cannot stage the V3 compatibility fallback: ${err}')
			exit(1)
		}
	}
	macos_v3_driver_run(forwarded_args)
	os.rm(fallback_file) or {}
	os.rmdir_all(c_error_dir) or {}
	exit(0)
}

fn macos_v3_c_error_report_dir(fallback_file string) string {
	return fallback_file + '.c_error'
}

fn macos_v3_fallback_file_for_pid() string {
	return os.join_path(os.vtmp_dir(), 'macos_v3_fallback_${os.getpid()}')
}

fn preserve_macos_v3_caller_environment_value(mut environment map[string]string, caller_environment map[string]string, name string, value_name string, present_name string) {
	if present := caller_environment[present_name] {
		if present in ['0', '1'] {
			environment[value_name] = caller_environment[value_name] or { '' }
			environment[present_name] = present
			return
		}
	}
	if value := caller_environment[name] {
		environment[value_name] = value
		environment[present_name] = '1'
	} else {
		environment[value_name] = ''
		environment[present_name] = '0'
	}
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

fn macos_v3_child_environment(vexe string, caller_environment map[string]string, dispatch_environment map[string]string) map[string]string {
	mut environment := dispatch_environment.clone()
	preserve_macos_v3_caller_environment_value(mut environment, caller_environment, 'VEXE', macos_v3_caller_vexe_env, macos_v3_caller_vexe_present_env)
	preserve_macos_v3_caller_environment_value(mut environment, caller_environment, 'VCHILD', macos_v3_caller_vchild_env, macos_v3_caller_vchild_present_env)
	preserve_macos_v3_caller_environment_value(mut environment, caller_environment, macos_v3_no_fallback_env, macos_v3_caller_no_fallback_env, macos_v3_caller_no_fallback_present_env)
	for private_name in [macos_v3_fallback_file_env, macos_v3_c_error_dir_env, macos_v3_retry_env] {
		environment.delete(private_name)
	}
	environment['VCHILD'] = 'true'
	environment['VEXE'] = os.real_path(vexe)
	environment[macos_v3_vhash_env] = @VHASH
	environment[macos_v3_vcurrent_hash_env] = @VCURRENTHASH
	environment[macos_v3_embedded_env] = '1'
	return environment
}

fn macos_v3_original_caller_environment(dispatch_environment map[string]string) map[string]string {
	mut caller_environment := dispatch_environment.clone()
	vexe_present := dispatch_environment[macos_v3_caller_vexe_present_env] or { '' }
	vchild_present := dispatch_environment[macos_v3_caller_vchild_present_env] or { '' }
	if vexe_present in ['0', '1'] && vchild_present in ['0', '1'] {
		restore_macos_v3_caller_environment_value(mut caller_environment, dispatch_environment, 'VEXE', macos_v3_caller_vexe_env, macos_v3_caller_vexe_present_env)
		restore_macos_v3_caller_environment_value(mut caller_environment, dispatch_environment, 'VCHILD', macos_v3_caller_vchild_env, macos_v3_caller_vchild_present_env)
	}
	no_fallback_present := dispatch_environment[macos_v3_caller_no_fallback_present_env] or { '' }
	if no_fallback_present in ['0', '1'] {
		restore_macos_v3_caller_environment_value(mut caller_environment, dispatch_environment, macos_v3_no_fallback_env, macos_v3_caller_no_fallback_env, macos_v3_caller_no_fallback_present_env)
	}
	for private_name in [macos_v3_fallback_file_env, macos_v3_c_error_dir_env, macos_v3_vhash_env,
		macos_v3_vcurrent_hash_env, macos_v3_embedded_env, macos_v3_retry_env,
		'V3_CRUN_BUILD_IDENTITY', 'V3_INTERNAL_RESTART', macos_v3_caller_vexe_env,
		macos_v3_caller_vexe_present_env, macos_v3_caller_vchild_env,
		macos_v3_caller_vchild_present_env, macos_v3_caller_no_fallback_env,
		macos_v3_caller_no_fallback_present_env] {
		caller_environment.delete(private_name)
	}
	return caller_environment
}

fn restore_macos_v3_caller_environment_value(mut caller_environment map[string]string, dispatch_environment map[string]string, name string, value_name string, present_name string) {
	if dispatch_environment[present_name] or { '' } == '1' {
		caller_environment[name] = dispatch_environment[value_name] or { '' }
	} else {
		caller_environment.delete(name)
	}
}
