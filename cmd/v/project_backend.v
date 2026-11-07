module main

import os
import v.pref
import v.vmod

// project_backends are the `backend` values that a project's v.mod can select.
const project_backends = ['c', 'fastc', 'arm64', 'x64', 'wasm']

// with_project_backend selects the backend named by the `backend` field of the input's v.mod,
// such as `backend: 'fastc'`, when the command line does not select one with `-b`.
fn with_project_backend(args []string) []string {
	compiler_args := compiler_args_before_program(args)
	if backend_option_given(compiler_args) {
		return args
	}
	backend := project_backend(compiler_input_path(compiler_args))
	if backend == '' {
		return args
	}
	mut result := ['-b', backend]
	result << args
	return result
}

// backend_option_given reports whether the compiler options in `args` select a backend.
fn backend_option_given(args []string) bool {
	if compiler_option_requested(args, '-b') || compiler_option_requested(args, '-backend') {
		return true
	}
	for arg in args {
		if arg.starts_with('-b=') || arg.starts_with('-backend=') {
			return true
		}
	}
	return false
}

// compiler_input_path returns the source file or directory that the compiler options in `args`
// compile, or '' for inline source, standard input, or a command line without an input.
fn compiler_input_path(args []string) string {
	mut option_value_follows := false
	mut command_seen := false
	for i, arg in args {
		if option_value_follows {
			option_value_follows = false
			continue
		}
		if is_inline_source_option(arg) || arg == '-' {
			return ''
		}
		if arg in ['-prof', '-profile'] {
			option_value_follows = v1_fallback_profile_option_consumes_value(args, i)
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			option_value_follows = true
			continue
		}
		if arg.starts_with('-') {
			continue
		}
		if !command_seen && arg in ['run', 'crun', 'build', 'test'] {
			command_seen = true
			continue
		}
		return arg
	}
	return ''
}

// project_backend returns the `backend` of the v.mod that owns `input`, or '' when there is
// none. An unreadable v.mod selects nothing, as for its other fields; an unknown backend is an
// error.
fn project_backend(input string) string {
	if input == '' || !os.exists(input) {
		return ''
	}
	dir := if os.is_dir(input) { input } else { os.dir(input) }
	root := pref.vmod_root_for_dir(dir) or { return '' }
	vmod_path := os.join_path_single(root, 'v.mod')
	manifest := vmod.from_file(vmod_path) or { return '' }
	if manifest.backend == '' {
		return ''
	}
	if manifest.backend !in project_backends {
		eprintln('${vmod_path}: unknown backend `${manifest.backend}`; expected ${project_backends.join(', ')}')
		exit(1)
	}
	return manifest.backend
}
