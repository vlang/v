module driver

import v.flat
import v.types

// start_program_instance_check starts the check of the instances of the
// program's generics in a compiler process with the arguments `args` of the
// build: Windows has no fork() (see start_program_instance_check_process).
fn start_program_instance_check(mut a flat.FlatAst, mut tc types.TypeChecker, args []string, is_checker_fixture bool, fatal_errors bool, message_limit int, skip_notices bool) ProgramInstanceCheck {
	return start_program_instance_check_process(args)
}

// program_instance_check_forks reports whether a build checks the instances of
// the program's generics in a fork of itself: never on Windows, where the check
// is a second compiler process.
fn program_instance_check_forks() bool {
	return false
}

// finish prints what the check printed and returns 1 when it found errors, -1
// when it gave no report.
fn (check ProgramInstanceCheck) finish() int {
	if isnil(check.process) {
		return -1
	}
	return check.finish_process()
}
