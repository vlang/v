module main

import os
import v.pref

const macos_v3_compat_c99_flag = '-macos-v3-compat-c99'
const macos_v3_internal_quiet_flag = '-macos-v3-internal-quiet'

// macos_v3_non_compilation_command lists the builtin commands that carry a path
// (or a directory) but are NOT compilation commands the V3 driver understands —
// it only knows `run`/`build`/`test`. An unrecognized command token such as
// `crun` or `build-module` would otherwise become V3's first input path and then
// collide with the real target. Both dispatch gates exclude these; keep the list
// in one place so they cannot drift. `test` is handled separately by each gate.
@[markused]
fn macos_v3_non_compilation_command(command string) bool {
	return command in ['build-module', 'crun', 'help', 'version', 'new', 'init', 'install', 'link',
		'list', 'outdated', 'remove', 'search', 'show', 'unlink', 'update', 'upgrade', 'vlib-docs',
		'interpret', 'get', 'translate']
}

// macos_v3_force_requested reports whether `-new-compiler` should hand this
// invocation to the embedded V3 compiler. It gates on `-old-compiler`
// precedence, options/modes V3 cannot honor yet, and whether the command is an
// actual compilation command (never `test` or external tools). Compiler bootstrap
// targets normally stay on V1, but explicit `-b fastc` owns those targets too: its
// direct emitter falls back to V3's checked C backend for the full compiler source.
// Both the Darwin dispatcher (where it overrides the default heuristic) and the
// non-macOS dispatcher (where it is the sole gate) rely on it, so it must stay
// platform neutral.
@[markused]
fn macos_v3_force_requested(command string, prefs &pref.Preferences) bool {
	if !prefs.new_compiler || prefs.old_compiler {
		return false
	}
	if v3_has_v1_only_preferences(prefs) || (prefs.gc_set_by_flag && prefs.gc_mode != .no_gc) {
		return false
	}
	if prefs.autofree && prefs.is_run {
		return false
	}
	if prefs.path == '' || command == 'test' || macos_v3_non_compilation_command(command)
		|| command in external_tools {
		return false
	}
	normalized_path := prefs.path.replace('\\', '/').trim_right('/')
	compiler_bootstrap := normalized_path == 'cmd/v' || normalized_path.starts_with('cmd/v/')
		|| normalized_path.contains('/cmd/v/') || normalized_path.ends_with('/cmd/v')
		|| normalized_path == 'vlib/v3/v3.v' || normalized_path.ends_with('/vlib/v3/v3.v')
	if compiler_bootstrap && !macos_v3_fastc_requested(prefs) {
		return false
	}
	return command in ['run', 'build'] || prefs.is_script || os.is_dir(prefs.path)
		|| normalized_path.ends_with('.v') || normalized_path.ends_with('.vsh')
}

fn macos_v3_fastc_requested(prefs &pref.Preferences) bool {
	mut selected_backend := ''
	for option in prefs.build_options {
		parts := option.fields()
		if parts.len == 2 && parts[0] in ['-b', '-backend'] {
			selected_backend = parts[1]
		}
	}
	return selected_backend == 'fastc'
}

// These helpers are shared by the native Darwin dispatcher and the default
// implementation selected while generating cross-platform VC sources, so this
// file has to stay platform neutral (no `_darwin.c.v` suffix). Keep them outside
// a top-level `$if macos {}` block so older bootstrap compilers can emit them
// while rebuilding V on macOS. `markused` suppresses unused-declaration notices
// when V3 compiles this file for other platforms.
@[markused]
fn macos_v3_has_v1_only_leading_option(args []string, command string) bool {
	mut i := 0
	for i < args.len {
		arg := args[i]
		if arg == '--' {
			return false
		}
		if arg in ['-message-limit', '-debug', '-debug-tcc', '-wasm-validate', '-wasm-stack-top',
			'-use-coroutines', '-checker-match-exhaustive-cutoff-limit', '-raw-vsh-tmp-prefix',
			'-c++', '-check-unused-fn-args', '-subsystem', '-translated-go', '-musl', '-glibc'] {
			return true
		}
		if macos_v3_leading_option_consumes_value(arg) {
			i += 2
			continue
		}
		if command.len > 0 && arg == command {
			return false
		}
		i++
	}
	return false
}

@[markused]
fn macos_v3_leading_option_consumes_value(option string) bool {
	return option in ['-wasm-stack-top', '-arch', '-assert', '-e', '-subsystem', '-icon', '--icon',
		'-seticon', '--seticon', '-gc', '-print_autofree_vars_in_fn', '-trace-fns', '-cov',
		'-coverage', '-profile-fns', '-bug-report-url', '-run-only', '-exclude', '-file-list',
		'-test-runner', '-dump-c-flags', '-dump-modules', '-dump-files', '-dump-defines',
		'-generate-c-project', '-macosx-version-min', '-os', '-printfn', '-cflags', '-ldflags',
		'-d', '-define', '-message-limit', '-thread-stack-size', '-cc', '-c++',
		'-checker-match-exhaustive-cutoff-limit', '-o', '-output', '-b', '-backend',
		'-compile-backend', '--compile-backend', '-path', '-bare-builtin-dir', '-custom-prelude',
		'-raw-vsh-tmp-prefix', '-cmain', '-line-info']
}

@[markused]
fn macos_v3_forwarded_args(prefs &pref.Preferences, raw_args []string) []string {
	// `-new-compiler` is consumed by cmd/v to select V3; it must not reach the V3
	// driver, which is already running and would reject it as an unknown option.
	mut forwarded_args := raw_args.filter(it != '-new-compiler')
	if prefs.enable_globals {
		for i, arg in forwarded_args {
			if arg == '--enable-globals' {
				forwarded_args[i] = '-enable-globals'
			}
		}
	}
	// V1 treats `x86` as an amd64 alias, while V3 reserves it for the 32-bit target.
	if prefs.arch == .amd64 && '-arch x86' in prefs.build_options {
		for i in 0 .. forwarded_args.len {
			if i + 1 < forwarded_args.len && forwarded_args[i] == '-arch'
				&& forwarded_args[i + 1] == 'x86' {
				forwarded_args[i + 1] = 'amd64'
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
	return forwarded_args
}
