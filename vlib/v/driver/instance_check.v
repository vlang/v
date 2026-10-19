module driver

import os
import v.ansi
import v.flat
import v.types

// instance_check_child_env marks a compiler process that a build started to
// check the instances of its generics (see start_program_instance_check_process).
const instance_check_child_env = 'V3_INTERNAL_INSTANCE_CHECK'

// instance_check_report_marker starts the report of such a process, on a line
// of its own: what it printed before repeats what the build printed itself.
const instance_check_report_marker = '\x1ev3 instance check report\x1e'

// ProgramInstanceCheck is the check of the instances of the program's generics
// that a child of a build runs beside the transform (see
// start_program_instance_check): the fields, methods and operators that the
// types of an instance lack, and the arrays and maps it reads as ones with
// other elements, which a check reports too.
struct ProgramInstanceCheck {
	pid     int         = -1
	output  i32         = -1           // the read end of what the child prints
	process &os.Process = unsafe { nil } // the child, when it is a compiler process (no fork)
}

// is_instance_check_child reports whether this compiler process only checks the
// instances of the generics of the build that started it.
fn is_instance_check_child() bool {
	return os.getenv(instance_check_child_env) == '1'
}

// start_program_instance_check_process starts the check of the instances of the
// program's generics where a build cannot fork: in a compiler process of its
// own, with the arguments `args` of the build. The check rewrites the tree and
// the checker's tables, which the build still needs as they are, and a process
// rebuilds both for itself. It parses and checks the program as the build did
// and stops where the build starts the check (run_program_instance_check_child).
// What it prints waits in a pipe until the build asks for it (finish).
fn start_program_instance_check_process(args []string) ProgramInstanceCheck {
	executable := os.executable()
	if executable == '' {
		return ProgramInstanceCheck{}
	}
	mut environment := os.environ()
	environment[instance_check_child_env] = '1'
	// The arguments already hold VFLAGS.
	environment['VFLAGS'] = ''
	// The check reports its own result, whatever it is, and has no C to retry
	// with V1, and the fallback marker of the build is not its own.
	environment[macos_v3_no_fallback_env] = '1'
	environment.delete(macos_v3_fallback_file_env)
	environment.delete(macos_v3_c_error_dir_env)
	environment.delete(macos_v3_source_manifest_env)
	// An executable that another build kept would run instead of the check.
	environment['V3_CACHE_DISABLE_PROGRAM_EXECUTABLE'] = '1'
	// Its output goes to a pipe: colors are what the build would print.
	environment['VCOLORS'] = if ansi.stderr_supports_escape_sequences() {
		'always'
	} else {
		'never'
	}
	// What the build printed so far stays where it was printed.
	flush_stdout()
	flush_stderr()
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_environment(environment)
	process.set_redirect_stdio_merged()
	process.run()
	if process.status != .running && process.status != .exited {
		process.close()
		return ProgramInstanceCheck{}
	}
	return ProgramInstanceCheck{
		process: process
	}
}

// finish_process is finish for a check in a compiler process: it prints the
// report of the check and returns 1 when the check found errors, -1 when the
// process gave no report.
fn (check ProgramInstanceCheck) finish_process() int {
	mut process := check.process
	output := process.stdout_slurp()
	process.wait()
	code := process.code
	process.close()
	start := output.last_index(instance_check_report_marker) or { return -1 }
	if code != 1 {
		return if code == 0 { 0 } else { -1 }
	}
	eprint(output[start + instance_check_report_marker.len..].trim_left('\r\n'))
	return 1
}

// run_program_instance_check_child is what a compiler process started by
// start_program_instance_check_process does where its build started it: it
// checks the instances of the program's generics as a check does, prints the
// errors after instance_check_report_marker and exits, 1 when there are errors.
@[noreturn]
fn run_program_instance_check_child(mut a flat.FlatAst, mut tc types.TypeChecker, is_checker_fixture bool, fatal_errors bool, message_limit int, skip_notices bool) {
	flush_stdout()
	flush_stderr()
	println(instance_check_report_marker)
	flush_stdout()
	check_concrete_generic_bodies_of_check(mut a, mut tc, false)
	code := if tc.errors.len > 0 { 1 } else { 0 }
	if code == 1 {
		print_type_diagnostics(a, []types.TypeError{}, tc.errors, is_checker_fixture, fatal_errors,
			false, message_limit, skip_notices)
	}
	flush_stdout()
	flush_stderr()
	exit(code)
}

// leave_instance_check_child ends a compiler process started by
// start_program_instance_check_process that did not get where its build
// started the check, without a report.
fn leave_instance_check_child() {
	if is_instance_check_child() {
		flush_stdout()
		flush_stderr()
		exit(0)
	}
}
