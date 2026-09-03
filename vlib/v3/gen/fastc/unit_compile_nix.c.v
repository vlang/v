module fastc

import os
import time

fn C.v_os_exec_capture_start(argv &&char, child_pid &int, read_fd &int) int

fn C.waitpid(pid int, status &int, options int) int

struct FastcUnitCompile {
	pid     int
	read_fd int
	object  string
}

// fastc_compile_c_units compiles the translation units to objects with
// concurrent TinyCC processes (started with posix_spawn, which is cheaper
// than forking the compiler process), and returns the object paths, or the
// output of the first compile that failed.
pub fn fastc_compile_c_units(tcc string, base_args []string, unit_paths []string) ![]string {
	bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
	sw := time.new_stopwatch()
	mut compiles := []FastcUnitCompile{cap: unit_paths.len}
	mut start_error := ''
	for unit_path in unit_paths {
		object := unit_path[..unit_path.len - 2] + '.o'
		mut args := [tcc]
		args << base_args
		args << ['-c', unit_path, '-o', object]
		mut pid := 0
		mut read_fd := -1
		if !fastc_start_capture(args, &pid, &read_fd) {
			start_error = 'could not start ${tcc} for ${unit_path}'
			break
		}
		compiles << FastcUnitCompile{
			pid: pid
			read_fd: read_fd
			object: object
		}
	}
	if bench_phases {
		eprintln('fastc-phase tcc.units_started ${sw.elapsed().microseconds()}us')
	}
	mut objects := []string{cap: unit_paths.len}
	mut failure := start_error
	for compile in compiles {
		// The merged output is drained before the wait, so a compiler that
		// prints more than a pipe buffer of diagnostics cannot block.
		output := os.fd_slurp(compile.read_fd).join('')
		os.fd_close(compile.read_fd)
		code := fastc_wait_exit_code(compile.pid)
		if bench_phases {
			eprintln('fastc-phase tcc.unit_done ${sw.elapsed().microseconds()}us ${os.file_name(compile.object)}')
		}
		if code != 0 && failure == '' {
			failure = if output.len > 0 { output } else { 'tcc failed on ${compile.object}' }
		}
		objects << compile.object
	}
	if failure != '' {
		return error(failure)
	}
	return objects
}

// fastc_run_command runs the program with the argument vector and returns
// its exit code and merged output, like cmdexec.run, but through posix_spawn
// and a blocking wait rather than a fork of this (large) process and a
// polling loop.
pub fn fastc_run_command(program string, args []string) os.Result {
	mut argv := [program]
	argv << args
	mut pid := 0
	mut read_fd := -1
	if !fastc_start_capture(argv, &pid, &read_fd) {
		return os.Result{
			exit_code: -1
			output: 'could not start ${program}'
		}
	}
	output := os.fd_slurp(read_fd).join('')
	os.fd_close(read_fd)
	return os.Result{
		exit_code: fastc_wait_exit_code(pid)
		output: output
	}
}

// fastc_start_capture starts the program of the argument vector with its
// stdout and stderr merged into a pipe, and reports whether it started.
fn fastc_start_capture(argv []string, pid &int, read_fd &int) bool {
	mut cargs := []&char{cap: argv.len + 1}
	for arg in argv {
		cargs << &char(arg.str)
	}
	cargs << &char(unsafe { nil })
	return C.v_os_exec_capture_start(cargs.data, pid, read_fd) == 0
}

// fastc_wait_exit_code waits for the process and returns its exit code; a
// process killed by a signal reports 128 plus the signal number.
fn fastc_wait_exit_code(pid int) int {
	mut status := 0
	for {
		C.errno = 0
		if C.waitpid(pid, &status, 0) != -1 {
			break
		}
		if C.errno != C.EINTR {
			return -1
		}
	}
	if (status & 0x7f) == 0 {
		return (status >> 8) & 0xff
	}
	return 128 + (status & 0x7f)
}
