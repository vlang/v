#!/usr/bin/env -S v run

// run-tests.vsh runs a V test suite with a filter, which is what makes a full
// suite workable: run one test while working, run everything before finishing.
//
//   v run scripts/run-tests.vsh src/lookup_test.v
//   v run scripts/run-tests.vsh src/ --fn 'test_login*'
//   v run scripts/run-tests.vsh src/ --list
//
// `--fn` matches test function names (VTEST_ONLY_FN); `--file` matches paths
// (VTEST_ONLY). With neither, everything runs.
//
// A run that selected no tests is reported as a failure, because an empty run and
// a passing run otherwise look the same.

import os
import term

fn main() {
	args := os.args[1..]
	if args.len == 0 {
		eprintln('usage: run-tests.vsh <path>... [--fn <pattern>] [--file <pattern>]')
		eprintln('  defaults to running every test under the given paths')
		exit(2)
	}
	mut targets := []string{}
	mut fn_filter := ''
	mut file_filter := ''
	mut stats := false
	mut i := 0
	for i < args.len {
		match args[i] {
			'--fn' {
				i++
				fn_filter = value_at(args, i, '--fn')
			}
			'--file' {
				i++
				file_filter = value_at(args, i, '--file')
			}
			'--stats' {
				stats = true
			}
			else {
				if args[i].starts_with('-') {
					eprintln('run-tests.vsh: unknown option `${args[i]}`')
					exit(2)
				}
				targets << args[i]
			}
		}
		i++
	}
	if targets.len == 0 {
		eprintln('run-tests.vsh: at least one test path is required')
		exit(2)
	}
	mut missing := 0
	for target in targets {
		if !os.exists(target) {
			eprintln('run-tests.vsh: `${target}` does not exist')
			missing++
		}
	}
	if missing > 0 {
		exit(2)
	}
	// The first reporter selector wins. VFLAGS precedes command-line options,
	// so prefix it while retaining the user's other compiler flags.
	restore_runner := set_env('VFLAGS', '-test-runner normal ' + os.getenv('VFLAGS'))
	defer {
		restore_runner()
	}
	// The filters go into the environment because the test runner reads them there,
	// not on its command line.
	restore := set_env('VTEST_ONLY_FN', fn_filter)
	defer {
		restore()
	}
	restore_file := set_env('VTEST_ONLY', file_filter)
	defer {
		restore_file()
	}
	mut failed := 0
	for target in targets {
		mut cmd := 'v'
		if stats {
			cmd += ' -stats'
		}
		cmd += ' -silent test ' + os.quoted_path(target)
		result := os.exec(os.split_args(cmd) or { panic(err) })
		if result.exit_code != 0 {
			failed++
			eprintln('run-tests.vsh: FAILED ${target}')
			eprint(result.output)
			continue
		}
		if selected_no_tests(result.output) {
			failed++
			eprintln('run-tests.vsh: no tests selected for ${target}')
			eprint(result.output)
			continue
		}
		println('run-tests.vsh: ok ${target}')
	}
	if failed > 0 {
		eprintln('run-tests.vsh: ${failed} target(s) failed')
		exit(1)
	}
	println('run-tests.vsh: all targets passed')
}

// selected_no_tests checks the runner's final summary, not earlier test output.
fn selected_no_tests(output string) bool {
	prefix := 'Summary for all V _test.v files: '
	mut summary := ''
	for line in term.strip_ansi(output).split_into_lines() {
		if line.starts_with(prefix) {
			summary = line[prefix.len..]
		}
	}
	return summary.starts_with('0 total.')
}

// value_at returns `args[i]`, or exits when the option has no value.
fn value_at(args []string, i int, option string) string {
	if i >= args.len {
		eprintln('run-tests.vsh: ${option} needs a pattern')
		exit(2)
	}
	return args[i]
}

// set_env sets `name` to `value` and returns a function that puts the previous
// value back.
//
// The filters live in the environment and this script may set both, so a filter
// left behind would silently narrow a later run in the same shell.
fn set_env(name string, value string) fn () {
	saved := os.getenv_opt(name) or { '' }
	if value == '' {
		os.unsetenv(name)
	} else {
		os.setenv(name, value, true)
	}
	return fn [name, saved] () {
		if saved == '' {
			os.unsetenv(name)
		} else {
			os.setenv(name, saved, true)
		}
	}
}
