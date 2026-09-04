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

fn fastc_render_c_unit_stdin(fd int, pieces []string, units &FastcUnitLayout, g int, first_unit int, end_unit int) {
	source := fastc_render_c_unit(pieces, units, g, first_unit, end_unit)
	fastc_write_unit_stdin(fd, source)
}

// fastc_prestart_c_units starts TinyCC processes that wait for their source on
// stdin. Starting this on a worker lets source generation hide process launch.
pub fn fastc_prestart_c_units(tcc string, base_args []string, build_dir string, unit_count int) FastcPrestartedCUnits {
	if unit_count < 2 {
		return FastcPrestartedCUnits{}
	}
	os.mkdir_all(build_dir) or { return FastcPrestartedCUnits{} }
	mut units := FastcPrestartedCUnits{
		build_dir: build_dir
		base_args: base_args.clone()
		unit_count: unit_count
		paths: []string{len: unit_count}
		objects: []string{len: unit_count}
		pids: []int{len: unit_count}
		read_fds: []int{len: unit_count, init: -1}
		write_fds: []int{len: unit_count, init: -1}
		active: true
	}
	for i in 0 .. unit_count {
		path := os.join_path_single(build_dir, 'src.unit${i}.c')
		object := path[..path.len - 2] + '.o'
		mut args := [tcc]
		args << base_args
		args << ['-c', '-', '-o', object]
		mut pid := 0
		mut read_fd := -1
		mut write_fd := -1
		if !fastc_start_capture_input(args, &pid, &read_fd, &write_fd) {
			fastc_discard_prestarted_c_units(mut units)
			return FastcPrestartedCUnits{}
		}
		units.paths[i] = path
		units.objects[i] = object
		units.pids[i] = pid
		units.read_fds[i] = read_fd
		units.write_fds[i] = write_fd
	}
	return units
}

// fastc_discard_prestarted_c_units closes and reaps unused prestarted TinyCC
// processes, then removes their private build directory.
pub fn fastc_discard_prestarted_c_units(mut units FastcPrestartedCUnits) {
	if !units.active {
		return
	}
	for fd in units.write_fds {
		if fd >= 0 {
			C.close(fd)
		}
	}
	for i, pid in units.pids {
		if pid <= 0 {
			continue
		}
		if i < units.read_fds.len && units.read_fds[i] >= 0 {
			_ = os.fd_slurp(units.read_fds[i])
			os.fd_close(units.read_fds[i])
		}
		fastc_wait_exit_code(pid)
	}
	units.active = false
	os.rmdir_all(units.build_dir) or {}
}

// fastc_begin_feed_prestarted_c_units starts copying rendered source to TinyCC
// processes that were launched while FastC generation was still running.
pub fn fastc_begin_feed_prestarted_c_units(mut prestarted FastcPrestartedCUnits, mut rendering FastcRenderingCUnits) !FastcFeedingCUnits {
	if !prestarted.active || rendering.paths != prestarted.paths
		|| rendering.workers.len != prestarted.unit_count {
		return error('invalid prestarted FastC unit layout')
	}
	mut writers := []thread{len: prestarted.unit_count}
	for i in rendering.order {
		writers[i] = spawn fastc_render_unit_stdin(prestarted.write_fds[i], rendering.workers[i])
		prestarted.write_fds[i] = -1
	}
	return FastcFeedingCUnits{
		paths: rendering.paths
		writers: writers
	}
}

// fastc_begin_render_prestarted_c_units renders directly into prestarted
// TinyCC processes, avoiding a second set of threads just to hand strings off.
pub fn fastc_begin_render_prestarted_c_units(mut prestarted FastcPrestartedCUnits, prefix string, pieces []string, units FastcUnitLayout, jobs int) !FastcFeedingCUnits {
	plan := fastc_c_unit_plan(prefix, pieces, units, jobs)
	if !prestarted.active || plan.paths != prestarted.paths
		|| plan.paths.len != prestarted.unit_count {
		return error('invalid prestarted FastC unit layout')
	}
	mut writers := []thread{len: prestarted.unit_count}
	for g in 0 .. plan.paths.len {
		writers[g] = spawn fastc_render_c_unit_stdin(prestarted.write_fds[g], pieces, &units, g, plan.first_units[g], plan.first_units[g + 1])
		prestarted.write_fds[g] = -1
	}
	return FastcFeedingCUnits{
		paths: plan.paths
		writers: writers
	}
}

// fastc_finish_prestarted_c_units waits for TinyCC and optionally loads each
// completed object into the prepared linker in translation-unit order.
pub fn fastc_finish_prestarted_c_units(mut prestarted FastcPrestartedCUnits, mut feeding FastcFeedingCUnits, prepared FastcPreparedUnits, mut prepared_link FastcPreparedLink, preload_link bool) ![]string {
	if !prestarted.active || feeding.paths != prestarted.paths
		|| feeding.writers.len != prestarted.unit_count
		|| prepared.entries.len != prestarted.unit_count {
		return error('invalid prestarted FastC unit layout')
	}
	mut failure := ''
	for i in 0 .. prestarted.unit_count {
		output := os.fd_slurp(prestarted.read_fds[i]).join('')
		os.fd_close(prestarted.read_fds[i])
		prestarted.read_fds[i] = -1
		feeding.writers[i].wait()
		code := fastc_wait_exit_code(prestarted.pids[i])
		prestarted.pids[i] = 0
		if code != 0 && failure == '' {
			failure = if output.len > 0 { output } else { 'tcc failed on ${prestarted.objects[i]}' }
		} else if code == 0 && preload_link && failure == '' {
			fastc_add_prepared_link_input(mut prepared_link, prestarted.objects[i]) or {
				failure = err.msg()
			}
		}
	}
	prestarted.active = false
	if failure != '' {
		return error(failure)
	}
	return prepared.objects
}

// fastc_compile_prestarted_rendering_c_units feeds and waits for translation
// units in one call. Callers with independent setup work can use the split
// begin/finish API to overlap that work with rendering and compilation.
pub fn fastc_compile_prestarted_rendering_c_units(mut prestarted FastcPrestartedCUnits, mut rendering FastcRenderingCUnits, prepared FastcPreparedUnits, mut prepared_link FastcPreparedLink, preload_link bool) ![]string {
	mut feeding := fastc_begin_feed_prestarted_c_units(mut prestarted, mut rendering)!
	return fastc_finish_prestarted_c_units(mut prestarted, mut feeding, prepared, mut prepared_link, preload_link)
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
