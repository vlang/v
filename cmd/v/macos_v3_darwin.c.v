module main

import os
import v.pref
import v.util

const macos_v3_vhash_env = 'V_MACOS_V3_VHASH'
const macos_v3_vcurrent_hash_env = 'V_MACOS_V3_VCURRENT_HASH'
const macos_v3_compat_c99_flag = '-macos-v3-compat-c99'
const macos_v3_caller_vexe_env = 'V_MACOS_V3_CALLER_VEXE'
const macos_v3_caller_vexe_present_env = 'V_MACOS_V3_CALLER_VEXE_PRESENT'
const macos_v3_caller_vchild_env = 'V_MACOS_V3_CALLER_VCHILD'
const macos_v3_caller_vchild_present_env = 'V_MACOS_V3_CALLER_VCHILD_PRESENT'
const macos_v3_embedded_env = 'V_MACOS_V3_EMBEDDED'

fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) ?MacosV3CErrorReport {
	if prefs.old_compiler {
		return none
	}
	if !macos_v3_driver_is_available() {
		return none
	}
	all_args := util.join_env_vflags_and_os_args()
	forwarded_args := all_args[1..]
	if !is_macos_v3_default_executable(os.executable())
		|| !is_macos_v3_relevant_command(command, prefs) {
		return none
	}
	return launch_macos_v3_compiler(prefs, forwarded_args)
}

fn is_macos_v3_default_executable(vexe string) bool {
	return os.base(vexe) in ['v', 'v.exe', 'vnew', 'vnew.exe']
}

fn is_macos_v3_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.old_compiler {
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
	if prefs.backend == .js_node && !prefs.backend_set_by_flag {
		// v.pref infers the Node.js backend from an explicit `.js` output name.
		// The embedded V3 driver receives raw arguments, so preserve that derived
		// preference explicitly instead of letting it fall back to C.
		forwarded_args.insert(0, 'js_node')
		forwarded_args.insert(0, '-b')
	}
	if macos_v3_compat_c99_flag !in forwarded_args {
		forwarded_args.insert(0, macos_v3_compat_c99_flag)
	}
	if prefs.skip_running && '-skip-running' !in forwarded_args {
		forwarded_args.insert(0, '-skip-running')
	}
	if !prefs.is_verbose && !prefs.is_stats && !prefs.show_timings && '-silent' !in forwarded_args {
		forwarded_args.insert(0, '-silent')
	}
	if '-no-memory-limit' !in forwarded_args && '--no-memory-limit' !in forwarded_args {
		forwarded_args.insert(0, '-no-memory-limit')
	}
	// An embedded V3 driver cannot restart itself by replacing the cmd/v process.
	if '-nocache' !in forwarded_args && '--no-cache' !in forwarded_args {
		forwarded_args.insert(0, '-nocache')
	}
	return forwarded_args
}

fn launch_macos_v3_compiler(prefs &pref.Preferences, raw_args []string) ?MacosV3CErrorReport {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	util.set_vroot_folder(vroot)
	forwarded_args := macos_v3_forwarded_args(prefs, raw_args)
	if prefs.is_verbose {
		println('Running macOS V3 compiler in process: ${util.args_quote_paths(forwarded_args)}')
	}
	environment := macos_v3_child_environment(vexe, os.environ())
	replace_macos_v3_process_environment(environment)
	macos_v3_driver_run(forwarded_args)
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

fn macos_v3_child_environment(vexe string, caller_environment map[string]string) map[string]string {
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
