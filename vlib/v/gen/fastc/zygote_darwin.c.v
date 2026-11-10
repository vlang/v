module fastc

import os
import strings
import time

$if !tinyc && !fastc_selfhost ? {
	#flag @VMODROOT/vlib/v/gen/fastc/libtcc_zygote_darwin.o
}

fn C.v_fastc_zygote_start(max_units int, control_fd &int, diagnostics_fd &int, done_fd &int, unit_fds &int) int

fn C.v_fastc_zygote_wait(pid int) int

fn C.v_fastc_zygote_kill(pid int)

const fastc_zygote_magic = u32(0x7a79676f)

fn fastc_start_zygote_process(max_units int) FastcZygoteProcess {
	mut unit_fds := []int{len: max_units, init: -1}
	mut control_fd := -1
	mut diagnostics_fd := -1
	mut done_fd := -1
	pid := C.v_fastc_zygote_start(max_units, &control_fd, &diagnostics_fd, &done_fd, &int(unit_fds.data))
	if pid <= 0 {
		return FastcZygoteProcess{}
	}
	return FastcZygoteProcess{
		pid:            pid
		control_fd:     control_fd
		diagnostics_fd: diagnostics_fd
		done_fd:        done_fd
		unit_fds:       unit_fds
		active:         true
	}
}

fn fastc_close_zygote_process(mut process FastcZygoteProcess) {
	C.close(process.control_fd)
	C.close(process.done_fd)
	for fd in process.unit_fds {
		if fd >= 0 {
			C.close(fd)
		}
	}
	fastc_finish_zygote_diagnostics(mut process)
	process.active = false
}

// fastc_end_zygote_process kills a zygote that holds a head no build uses.
fn fastc_end_zygote_process(process FastcZygoteProcess) bool {
	C.v_fastc_zygote_kill(process.pid)
	mut ended := process
	fastc_close_zygote_process(mut ended)
	return true
}

fn fastc_start_libtcc_zygote(max_units int, head_path string) FastcZygote {
	if max_units < 2 || max_units > 64 {
		return FastcZygote{}
	}
	fresh := fastc_start_zygote_process(max_units)
	if !fresh.active {
		return FastcZygote{}
	}
	mut zygote := FastcZygote{
		fresh:     fresh
		head_path: head_path
		active:    true
	}
	if head_path != '' && os.is_file(head_path) {
		zygote.guess = fastc_start_zygote_process(max_units)
		if zygote.guess.active {
			// Both zygotes have forked before a reader thread starts. Drain the
			// speculative head's warnings before waiting for its feeder later.
			fastc_start_zygote_diagnostics(mut zygote.guess)
			zygote.guess_feed = spawn fastc_feed_zygote_guess(zygote.guess.control_fd, head_path)
		}
	}
	return zygote
}

fn fastc_discard_libtcc_zygote(mut zygote FastcZygote) {
	if !zygote.active {
		return
	}
	zygote.active = false
	if zygote.guess.active {
		// Its feeder may be blocked on the pipe that the process reads.
		C.v_fastc_zygote_kill(zygote.guess.pid)
		zygote.guess_feed.wait()
		fastc_close_zygote_process(mut zygote.guess)
	}
	if zygote.fresh.active {
		// A zygote whose control pipe closes before a job exits at once.
		pid := zygote.fresh.pid
		fastc_close_zygote_process(mut zygote.fresh)
		C.v_fastc_zygote_wait(pid)
	}
}

fn fastc_zygote_job_u32(mut job []u8, value u32) {
	job << u8(value)
	job << u8(value >> 8)
	job << u8(value >> 16)
	job << u8(value >> 24)
}

// fastc_zygote_job encodes what a zygote reads before the shared head.
fn fastc_zygote_job(options string) []u8 {
	mut job := []u8{cap: options.len + 8}
	fastc_zygote_job_u32(mut job, fastc_zygote_magic)
	fastc_zygote_job_u32(mut job, u32(options.len))
	job << options.bytes()
	return job
}

// fastc_feed_zygote_guess gives a zygote the options and the head that the
// previous build kept, and returns them. The pipe stays open for the marker.
fn fastc_feed_zygote_guess(fd int, head_path string) FastcZygoteHead {
	content := os.read_file(head_path) or { return FastcZygoteHead{} }
	head := fastc_parse_zygote_head(content) or { return FastcZygoteHead{} }
	job := fastc_zygote_job(head.options)
	if !fastc_write_stdin_bytes(fd, job.data, job.len)
		|| !fastc_write_stdin_bytes(fd, head.text.str, head.text.len) {
		return FastcZygoteHead{}
	}
	return FastcZygoteHead{
		options: head.options
		text:    head.text
		fed:     true
	}
}

// fastc_zygote_marker_line ends the head: the units part where TinyCC reports it.
fn fastc_zygote_marker_line(unit_count int, object_prefix string) string {
	return '\n${fastc_zygote_marker} ${unit_count} ${object_prefix}\n\n'
}

// fastc_write_zygote_head gives a zygote its job and streams the declarations
// that every unit sees. It keeps a large head in `head_path` for the next build.
fn fastc_write_zygote_head(fd int, options string, pieces []string, units &FastcUnitLayout, marker string, head_path string) bool {
	mut out := strings.new_builder(fastc_unit_stdin_buffer_size)
	mut kept := strings.new_builder(if head_path != '' { 8 * 1024 * 1024 } else { 0 })
	defer {
		// Only this worker owns the buffers; all writes completed before they are freed.
		unsafe { out.free() }
		unsafe { kept.free() }
	}
	job := fastc_zygote_job(options)
	mut written := fastc_write_stdin_bytes(fd, job.data, job.len)
	slots := fastc_zygote_extern_slots(units)
	for k in 0 .. units.head_end {
		piece := if slots[k] >= 0 { units.extern_texts[slots[k]] } else { pieces[k] }
		if written && !fastc_append_c_unit_piece(fd, mut out, piece) {
			written = false
		}
		if head_path != '' {
			kept.write_string(piece)
		}
	}
	if written {
		written = fastc_append_c_unit_piece(fd, mut out, marker) && fastc_flush_c_unit_stdin(fd, mut out)
	}
	C.close(fd)
	if head_path != '' && kept.len >= fastc_zygote_head_bytes {
		// The units compile while the head is stored. A build that loses the
		// race for the file keeps the head of the other one.
		staged := '${head_path}.tmp.${os.getpid()}'
		mut file := os.create(staged) or { return written }
		file.write_string(fastc_zygote_head_prefix(options)) or {}
		file.write(kept) or {}
		file.close()
		os.mv(staged, head_path) or { os.rm(staged) or {} }
	}
	return written
}

// fastc_write_zygote_marker ends a head that the zygote already holds.
fn fastc_write_zygote_marker(fd int, marker string) bool {
	written := fastc_write_stdin_bytes(fd, marker.str, marker.len)
	C.close(fd)
	return written
}

// fastc_write_zygote_unit streams what one unit adds to the shared head: its
// items of the plan and, in the first unit, the definitions of what the head
// declares `extern`.
fn fastc_write_zygote_unit(fd int, pieces []string, units &FastcUnitLayout, plan &FastcZygotePlan, g int) bool {
	mut out := strings.new_builder(fastc_unit_stdin_buffer_size)
	defer {
		C.close(fd)
		// Only this worker owns the buffer; all writes completed before it is freed.
		unsafe { out.free() }
	}
	if g == 0 {
		if !fastc_append_c_unit_piece(fd, mut out, plan.definitions) {
			return false
		}
	}
	if !fastc_append_c_unit_piece(fd, mut out, '\n') {
		return false
	}
	solo_items := plan.solo_pieces.len - 1
	for item in plan.unit_items[g] .. plan.unit_items[g + 1] {
		mut first_piece := 0
		mut end_piece := 0
		if item < solo_items {
			first_piece = plan.solo_pieces[item]
			end_piece = plan.solo_pieces[item + 1]
		} else {
			first_piece = units.unit_starts[item - solo_items]
			end_piece = units.unit_starts[item - solo_items + 1]
		}
		for k in first_piece .. end_piece {
			if !fastc_append_c_unit_piece(fd, mut out, pieces[k]) {
				return false
			}
		}
	}
	return fastc_flush_c_unit_stdin(fd, mut out)
}

fn fastc_read_zygote_diagnostics(fd int) string {
	output := os.fd_slurp(fd).join('')
	os.fd_close(fd)
	return output
}

fn fastc_start_zygote_diagnostics(mut process FastcZygoteProcess) {
	fd := process.diagnostics_fd
	process.diagnostics_fd = -1
	process.diagnostics_reader = spawn fastc_read_zygote_diagnostics(fd)
	process.diagnostics_started = true
}

fn fastc_finish_zygote_diagnostics(mut process FastcZygoteProcess) string {
	if process.diagnostics_started {
		process.diagnostics_started = false
		return process.diagnostics_reader.wait()
	}
	if process.diagnostics_fd >= 0 {
		C.close(process.diagnostics_fd)
		process.diagnostics_fd = -1
	}
	return ''
}

fn fastc_compile_libtcc_zygote_c_units(mut zygote FastcZygote, base_args []string, objects []string, pieces []string, units FastcUnitLayout, plan FastcZygotePlan, mut prepared_link FastcPreparedLink, preload_link bool) ! {
	unit_count := objects.len
	if !zygote.active || !zygote.fresh.active || unit_count < 2
		|| unit_count > zygote.fresh.unit_fds.len || plan.unit_items.len != unit_count + 1 {
		return error('invalid FastC unit zygote layout')
	}
	for g, object in objects {
		if object != '${plan.object_prefix}${g}.o' {
			return error('invalid FastC unit zygote objects')
		}
	}
	bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
	sw := time.new_stopwatch()
	zygote.active = false
	options := fastc_libtcc_options(base_args)
	marker := fastc_zygote_marker_line(unit_count, plan.object_prefix)
	// The zygote that parsed the previous build's head has this build's head when
	// the options and the text are the same; it only waits for the marker.
	mut guessed := false
	mut guess_enders := []thread bool{cap: 1}
	if zygote.guess.active {
		guess := zygote.guess_feed.wait()
		guessed = guess.fed && guess.options == options
			&& fastc_zygote_head_matches(guess.text, pieces, &units)
		if !guessed {
			// The other zygote parses the new head while this one is ended.
			guess_enders << spawn fastc_end_zygote_process(zygote.guess)
			zygote.guess.active = false
		}
	}
	mut process := zygote.fresh
	mut head_written := true
	mut head_writers := []thread bool{cap: 1}
	if guessed {
		process = zygote.guess
		zygote.guess.active = false
		pid := zygote.fresh.pid
		fastc_close_zygote_process(mut zygote.fresh)
		C.v_fastc_zygote_wait(pid)
		head_written = fastc_write_zygote_marker(process.control_fd, marker)
	} else {
		zygote.fresh.active = false
		fastc_start_zygote_diagnostics(mut process)
		// The zygote parses the head while the unit writers fill their pipes.
		head_writers << spawn fastc_write_zygote_head(process.control_fd, options, pieces, &units, marker, zygote.head_path)
	}
	mut unit_writers := []thread bool{cap: unit_count}
	for g in 0 .. unit_count {
		unit_writers << spawn fastc_write_zygote_unit(process.unit_fds[g], pieces, &units, &plan, g)
	}
	for g in unit_count .. process.unit_fds.len {
		C.close(process.unit_fds[g])
	}
	if bench_phases {
		head := if guessed { 'guessed' } else { 'parsed' }
		eprintln('fastc-phase tcc.units_started ${sw.elapsed().microseconds()}us zygote=${unit_count} head=${head}')
	}
	mut done := []bool{len: unit_count}
	mut next_input := 0
	mut link_failure := ''
	mut finished := []u8{len: 64}
	for {
		got := int(C.read(process.done_fd, finished.data, uint(finished.len)))
		if got < 0 && C.errno == C.EINTR {
			continue
		}
		if got <= 0 {
			break
		}
		for i in 0 .. got {
			unit := int(finished[i])
			if unit < unit_count {
				done[unit] = true
				if bench_phases {
					eprintln('fastc-phase tcc.unit_done ${sw.elapsed().microseconds()}us ${os.file_name(objects[unit])}')
				}
			}
		}
		for preload_link && link_failure == '' && next_input < unit_count && done[next_input] {
			fastc_add_prepared_link_input(mut prepared_link, objects[next_input]) or {
				link_failure = err.msg()
			}
			next_input++
		}
	}
	C.close(process.done_fd)
	mut written := head_written
	for head_writer in head_writers {
		if !head_writer.wait() {
			written = false
		}
	}
	for unit_writer in unit_writers {
		if !unit_writer.wait() {
			written = false
		}
	}
	code := C.v_fastc_zygote_wait(process.pid)
	diagnostics := fastc_finish_zygote_diagnostics(mut process)
	for guess_ender in guess_enders {
		guess_ender.wait()
	}
	mut all_done := true
	for unit_done in done {
		if !unit_done {
			all_done = false
		}
	}
	if code != 0 || !all_done {
		if diagnostics.len > 0 {
			return error(diagnostics)
		}
		if !written {
			return error('could not write FastC source for ${objects[0]}')
		}
		return error('tcc failed on ${objects[0]}')
	}
	if link_failure != '' {
		return error(link_failure)
	}
}
