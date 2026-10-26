module driver

import os
import v.flat
import v.types
import v.workers

fn C.waitpid(pid int, status &int, options int) int

// start_program_instance_check starts a child of the build that checks the
// instances of the program's generics as a check does
// (check_concrete_generic_bodies_of_check). That check rewrites the tree, which
// the build still needs as it is, and it can run beside the transform: the
// build asks for its result before C generation (finish). What the child prints
// waits in a pipe until then. The child is a fork of the build, which has the
// tree and the checker as they are; V_CHECK_INSTANCES_IN_BUILD=process makes it
// a compiler process with the arguments `args` of the build instead, as on
// Windows (see start_program_instance_check_process).
fn start_program_instance_check(mut a flat.FlatAst, mut tc types.TypeChecker, args []string, is_checker_fixture bool, fatal_errors bool, message_limit int, skip_notices bool) ProgramInstanceCheck {
	if os.getenv('V_CHECK_INSTANCES_IN_BUILD') == 'process' {
		return start_program_instance_check_process(args)
	}
	mut fds := [2]i32{}
	if C.pipe(&fds[0]) != 0 {
		return ProgramInstanceCheck{}
	}
	// What the build printed so far must not be printed again by the child.
	flush_stdout()
	flush_stderr()
	pid := os.fork()
	if pid < 0 {
		C.close(fds[0])
		C.close(fds[1])
		return ProgramInstanceCheck{}
	}
	if pid == 0 {
		C.close(fds[0])
		C.dup2(fds[1], 1)
		C.dup2(fds[1], 2)
		// The parent's workers stayed in the parent: the child's pools start their own.
		workers.note_fork()
		check_concrete_generic_bodies_of_check(mut a, mut tc, true)
		code := if tc.errors.len > 0 { 1 } else { 0 }
		if code == 1 {
			print_type_diagnostics(a, []types.TypeError{}, tc.errors, is_checker_fixture,
				fatal_errors, false, message_limit, skip_notices)
		}
		flush_stdout()
		flush_stderr()
		// Not exit(): the parent's exit handlers, such as the removal of its
		// temporary files, belong to the build.
		C._exit(code)
	}
	C.close(fds[1])
	return ProgramInstanceCheck{
		pid:    pid
		output: fds[0]
	}
}

// program_instance_check_forks reports whether a build checks the instances of
// the program's generics in a fork of itself, which costs little more than the
// check: then it also checks the instances of library generics that the
// program's own code asks for (see types.TypeChecker.check_program_lib_instances).
fn program_instance_check_forks() bool {
	return os.getenv('V_CHECK_INSTANCES_IN_BUILD') != 'process'
}

// finish prints what the check printed and returns the exit code of its child:
// 1 when it found errors, -1 when there was no child.
fn (check ProgramInstanceCheck) finish() int {
	if !isnil(check.process) {
		return check.finish_process()
	}
	if check.pid < 0 {
		return -1
	}
	mut buf := []u8{len: 65536}
	for {
		n := C.read(check.output, buf.data, uint(buf.len))
		if n < 0 && C.errno == C.EINTR {
			continue
		}
		if n <= 0 {
			break
		}
		eprint(buf[..n].bytestr())
	}
	C.close(check.output)
	mut status := 0
	for C.waitpid(check.pid, &status, 0) < 0 {
		if C.errno != C.EINTR {
			return -1
		}
	}
	if status & 0x7f == 0 {
		return (status >> 8) & 0xff
	}
	return 128 + (status & 0x7f)
}
