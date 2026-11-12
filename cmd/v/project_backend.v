module main

import os
import v.driver
import v.pref
import v.vmod

// project_backends are the `backend` values that a project's v.mod can select.
const project_backends = ['c', 'fastc', 'arm64', 'amd64', 'wasm']

// with_project_backend selects the backend named by the `backend` field of the input's v.mod,
// such as `backend: 'fastc'`, when the command line does not select one with `-b`. The driver
// reads `driver.project_backend_flag` before it as "the project's default backend".
fn with_project_backend(args []string) []string {
	compiler_args := compiler_args_before_program(args)
	if backend_option_given(compiler_args) {
		return args
	}
	input := compiler_input_path(compiler_args)
	if driver.is_generated_source(input) {
		// `-e` code and standard input belong to no project, even in a project folder.
		return args
	}
	backend := project_backend(input)
	if backend == '' {
		return args
	}
	mut result := [driver.project_backend_flag, '-b', backend]
	result << args
	return result
}

// project_test_runs returns the command lines that run `v test` for `args`, where `test` is
// at `command_index`. The test runner selects files and evaluates their `// vtest build:`
// constraints for the backend in the options before `test`, so the targets of each project
// backend get a run of their own with that backend there. Runs follow the first target of each
// backend; the targets keep their order. An explicit `-b` makes one run of all targets.
fn project_test_runs(args []string, command_index int) [][]string {
	prefix := args[..command_index]
	if backend_option_given(prefix) {
		return [args]
	}
	mut options := []string{}
	mut backends := []string{}
	mut targets := [][]string{}
	mut value_follows := false
	for arg in args[command_index + 1..] {
		if value_follows || arg.starts_with('-') {
			options << arg
			value_follows = !value_follows && arg == '-run-only'
			continue
		}
		backend := project_backend(arg)
		mut index := backends.index(backend)
		if index < 0 {
			backends << backend
			targets << []string{}
			index = backends.len - 1
		}
		targets[index] << arg
	}
	if backends.len <= 1 {
		backend := if backends.len == 0 { '' } else { backends[0] }
		if backend == '' {
			return [args]
		}
		mut run := ['-b', backend]
		run << args
		return [run]
	}
	mut runs := [][]string{}
	for i, backend in backends {
		mut run := []string{}
		if backend != '' {
			run << ['-b', backend]
		}
		run << prefix
		run << 'test'
		run << options
		run << targets[i]
		runs << run
	}
	return runs
}

// run_project_tests runs each command line of project_test_runs with this launcher, one after
// another, and returns 1 when any of them failed. Fail-fast, from `-fail-fast` or, as in the test
// runner, from `VTEST_FAIL_FAST=1`, stops after the first failed run.
fn run_project_tests(runs [][]string) int {
	fail_fast_env := os.getenv('VTEST_FAIL_FAST') == '1'
	// The command lines already hold the options of VFLAGS; the runs must not add them again.
	mut environment := os.environ()
	environment.delete('VFLAGS')
	mut status := 0
	for run in runs {
		mut process := os.new_process(os.executable())
		process.set_args(run)
		process.set_environment(environment)
		process.wait()
		failed := process.code != 0
		process.close()
		if failed {
			status = 1
			if fail_fast_env || '-fail-fast' in run {
				break
			}
		}
	}
	return status
}

// backend_option_given reports whether the compiler options in `args` select a backend.
fn backend_option_given(args []string) bool {
	if compiler_option_requested(args, '-b') || compiler_option_requested(args, '-backend')
		|| compiler_option_requested(args, '-native') {
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
	backend := match manifest.backend {
		'x64', 'x86_64', 'x86-64' { 'amd64' }
		else { manifest.backend }
	}
	if backend !in project_backends {
		eprintln('${vmod_path}: unknown backend `${manifest.backend}`; expected ${project_backends.join(', ')}')
		exit(1)
	}
	return backend
}
