module fastc

import os
import time

fn C.v_os_exec_capture_start(argv &&char, child_pid &int, read_fd &int) int

fn C.v_os_exec_capture_input_start(argv &&char, child_pid &int, read_fd &int, write_fd &int) int

fn C.v_os_fd_write_all(fd int, data &char, len usize)

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
pub fn fastc_compile_c_units(tcc string, base_args []string, unit_paths []string, prepared FastcPreparedUnits) ![]string {
	bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
	sw := time.new_stopwatch()
	mut compiles := []FastcUnitCompile{cap: unit_paths.len}
	mut start_error := ''
	for i in fastc_unit_compile_order(unit_paths, prepared) {
		unit_path := unit_paths[i]
		entry := prepared.entries[i]
		object := entry.object
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
	}
	if failure != '' {
		return error(failure)
	}
	for entry in prepared.entries {
		fastc_publish_unit_cache(entry)
	}
	return prepared.objects
}

fn fastc_stream_unit_compile_order(sources []string) []int {
	mut order := []int{cap: sources.len}
	mut sizes := []int{cap: sources.len}
	for i, source in sources {
		order << i
		sizes << source.len
		mut at := order.len - 1
		for at > 0 && sizes[at - 1] < source.len {
			order[at] = order[at - 1]
			sizes[at] = sizes[at - 1]
			at--
		}
		order[at] = i
		sizes[at] = source.len
	}
	return order
}

fn fastc_write_unit_stdin(fd int, source string) {
	C.v_os_fd_write_all(fd, &char(source.str), usize(source.len))
	C.close(fd)
}

fn fastc_render_unit_stdin(fd int, worker thread string) {
	source := worker.wait()
	fastc_write_unit_stdin(fd, source)
}

// fastc_compile_rendering_c_units starts concurrent TinyCC processes while
// their translation units are still being rendered, then streams each unit
// as soon as its render worker completes.
pub fn fastc_compile_rendering_c_units(tcc string, base_args []string, mut rendering FastcRenderingCUnits, prepared FastcPreparedUnits, mut prepared_link FastcPreparedLink, preload_link bool) ![]string {
	if rendering.paths.len != rendering.workers.len
		|| prepared.entries.len != rendering.paths.len {
		return error('invalid rendering FastC unit layout')
	}
	bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
	sw := time.new_stopwatch()
	mut compiles := []FastcUnitCompile{len: rendering.paths.len}
	mut writers := []thread{len: rendering.paths.len}
	mut handed := []bool{len: rendering.paths.len}
	mut start_error := ''
	for i in rendering.order {
		object := prepared.entries[i].object
		mut args := [tcc]
		args << base_args
		args << ['-c', '-', '-o', object]
		mut pid := 0
		mut read_fd := -1
		mut write_fd := -1
		if !fastc_start_capture_input(args, &pid, &read_fd, &write_fd) {
			start_error = 'could not start ${tcc} for ${rendering.paths[i]}'
			break
		}
		compiles[i] = FastcUnitCompile{
			pid: pid
			read_fd: read_fd
			object: object
		}
		handed[i] = true
		writers[i] = spawn fastc_render_unit_stdin(write_fd, rendering.workers[i])
	}
	if bench_phases {
		eprintln('fastc-phase tcc.units_started ${sw.elapsed().microseconds()}us streamed=rendering')
	}
	mut failure := start_error
	for i, compile in compiles {
		if !handed[i] {
			continue
		}
		output := os.fd_slurp(compile.read_fd).join('')
		os.fd_close(compile.read_fd)
		writers[i].wait()
		code := fastc_wait_exit_code(compile.pid)
		if bench_phases {
			eprintln('fastc-phase tcc.unit_done ${sw.elapsed().microseconds()}us ${os.file_name(compile.object)}')
		}
		if code != 0 && failure == '' {
			failure = if output.len > 0 { output } else { 'tcc failed on ${compile.object}' }
		} else if code == 0 && preload_link && failure == '' {
			fastc_add_prepared_link_input(mut prepared_link, compile.object) or {
				failure = err.msg()
			}
		}
	}
	for i, was_handed in handed {
		if !was_handed {
			rendering.workers[i].wait()
		}
	}
	if failure != '' {
		return error(failure)
	}
	return prepared.objects
}

// fastc_compile_c_unit_texts streams in-memory translation units to concurrent
// TinyCC processes. This avoids a temporary-file write/read round trip and
// overlaps feeding earlier processes with starting the remaining ones.
pub fn fastc_compile_c_unit_texts(tcc string, base_args []string, unit_paths []string, sources []string, prepared FastcPreparedUnits) ![]string {
	if unit_paths.len != sources.len || prepared.entries.len != sources.len {
		return error('invalid streamed FastC unit layout')
	}
	bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
	sw := time.new_stopwatch()
	mut compiles := []FastcUnitCompile{cap: unit_paths.len}
	mut writers := []thread{cap: unit_paths.len}
	mut start_error := ''
	for i in fastc_stream_unit_compile_order(sources) {
		object := prepared.entries[i].object
		mut args := [tcc]
		args << base_args
		args << ['-c', '-', '-o', object]
		mut pid := 0
		mut read_fd := -1
		mut write_fd := -1
		if !fastc_start_capture_input(args, &pid, &read_fd, &write_fd) {
			start_error = 'could not start ${tcc} for ${unit_paths[i]}'
			break
		}
		compiles << FastcUnitCompile{
			pid: pid
			read_fd: read_fd
			object: object
		}
		writers << spawn fastc_write_unit_stdin(write_fd, sources[i])
	}
	if bench_phases {
		eprintln('fastc-phase tcc.units_started ${sw.elapsed().microseconds()}us streamed=1')
	}
	mut failure := start_error
	for i, compile in compiles {
		output := os.fd_slurp(compile.read_fd).join('')
		os.fd_close(compile.read_fd)
		writers[i].wait()
		code := fastc_wait_exit_code(compile.pid)
		if bench_phases {
			eprintln('fastc-phase tcc.unit_done ${sw.elapsed().microseconds()}us ${os.file_name(compile.object)}')
		}
		if code != 0 && failure == '' {
			failure = if output.len > 0 { output } else { 'tcc failed on ${compile.object}' }
		}
	}
	if failure != '' {
		return error(failure)
	}
	return prepared.objects
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

fn fastc_start_capture_input(argv []string, pid &int, read_fd &int, write_fd &int) bool {
	$if macos {
		mut cargs := []&char{cap: argv.len + 1}
		for arg in argv {
			cargs << &char(arg.str)
		}
		cargs << &char(unsafe { nil })
		return C.v_os_exec_capture_input_start(cargs.data, pid, read_fd, write_fd) == 0
	} $else {
		return false
	}
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
