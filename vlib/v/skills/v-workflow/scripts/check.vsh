#!/usr/bin/env -S v run

// check.vsh runs the V gate on one or more paths: does it type-check, is it
// formatted, and does vet complain. It is the loop from the skill, in one command.
//
// Every path is a file or a directory. Pass `--shared` when the target is library
// code rather than a `main` module, because `v -check` rejects a library without
// it.
//
//   v run scripts/check.vsh vlib/v/skills/ --shared
//   v run scripts/check.vsh src/main.v
//
// Exits non-zero if any of the three fails, so it can gate a commit.

import os

fn main() {
	args := os.args[1..]
	if args.len == 0 {
		eprintln('usage: check.vsh <path>... [--shared]')
		eprintln('  reports: does it type-check, is it formatted, does vet pass')
		exit(2)
	}
	mut shared := false
	mut targets := []string{}
	for arg in args {
		if arg == '--shared' {
			shared = true
			continue
		}
		if arg.starts_with('-') {
			eprintln('check.vsh: unknown option `${arg}`')
			exit(2)
		}
		targets << arg
	}
	mut failures := 0
	mut missing := 0
	for target in targets {
		if !os.exists(target) {
			eprintln('check.vsh: `${target}` does not exist')
			missing++
			continue
		}
		failures += run_step('type-check', target, check_command(target, shared))
		failures += run_step('formatted', target, fmt_command(target))
		failures += run_step('vet', target, vet_command(target))
	}
	if missing > 0 {
		eprintln('check.vsh: ${missing} path(s) did not exist')
		exit(2)
	}
	if failures > 0 {
		eprintln('check.vsh: ${failures} check(s) failed')
		exit(1)
	}
	println('check.vsh: everything passed')
}

// check_command returns the `v -check` invocation for `target`.
//
// A directory or a library module needs `-shared`; `v -check` refuses a library
// with "project must include a `main` module", and passing it for a program is
// harmless.
fn check_command(target string, shared bool) string {
	mut cmd := 'v -check'
	if shared || os.is_dir(target) {
		cmd += ' -shared'
	}
	return cmd + ' ' + os.quoted_path(target)
}

// fmt_command returns the `v fmt -verify` invocation for `target`.
fn fmt_command(target string) string {
	return 'v fmt -verify ' + os.quoted_path(target)
}

// vet_command returns the `v vet -W` invocation for `target`.
fn vet_command(target string) string {
	return 'v vet -W ' + os.quoted_path(target)
}

// run_step runs one check and reports it, counting a failure rather than exiting
// so that one bad path does not hide the state of the others.
fn run_step(label string, target string, cmd string) int {
	result := os.exec(os.split_args(cmd) or { panic(err) })
	if result.exit_code == 0 {
		println('  ok       ${label}: ${target}')
		return 0
	}
	eprintln('  FAILED   ${label}: ${target}')
	// The compiler's own output is the reason, so pass it through rather than
	// summarising it away.
	eprint(result.output)
	eprintln('  command: ${cmd}')
	return 1
}
