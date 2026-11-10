module fastc

import os
import strings
import time

$if !tinyc && !fastc_selfhost ? {
	#flag @VMODROOT/vlib/v/gen/fastc/libtcc_zygote_darwin.o
}

fn C.v_fastc_zygote_start(max_units int, control_fd &int, diagnostics_fd &int, done_fd &int, unit_fds &int) int

fn C.v_fastc_zygote_wait(pid int) int

const fastc_zygote_magic = u32(0x7a79676f)

fn fastc_start_libtcc_zygote(max_units int) FastcZygote {
	if max_units < 2 || max_units > 64 {
		return FastcZygote{}
	}
	mut unit_fds := []int{len: max_units, init: -1}
	mut control_fd := -1
	mut diagnostics_fd := -1
	mut done_fd := -1
	pid := C.v_fastc_zygote_start(max_units, &control_fd, &diagnostics_fd, &done_fd, &int(unit_fds.data))
	if pid <= 0 {
		return FastcZygote{}
	}
	return FastcZygote{
		pid:            pid
		control_fd:     control_fd
		diagnostics_fd: diagnostics_fd
		done_fd:        done_fd
		unit_fds:       unit_fds
		active:         true
	}
}

fn fastc_discard_libtcc_zygote(mut zygote FastcZygote) {
	if !zygote.active {
		return
	}
	zygote.active = false
	// A zygote whose control pipe closes before a job exits at once.
	C.close(zygote.control_fd)
	C.close(zygote.diagnostics_fd)
	C.close(zygote.done_fd)
	for fd in zygote.unit_fds {
		if fd >= 0 {
			C.close(fd)
		}
	}
	C.v_fastc_zygote_wait(zygote.pid)
}

fn fastc_zygote_job_u32(mut job []u8, value u32) {
	job << u8(value)
	job << u8(value >> 8)
	job << u8(value >> 16)
	job << u8(value >> 24)
}

fn fastc_zygote_job_text(mut job []u8, text string) {
	fastc_zygote_job_u32(mut job, u32(text.len))
	job << text.bytes()
}

// fastc_zygote_job encodes what the zygote reads before the shared head: the
// TinyCC options and one object path per unit.
fn fastc_zygote_job(options string, objects []string) []u8 {
	mut job := []u8{cap: options.len + objects.len * 128 + 16}
	fastc_zygote_job_u32(mut job, fastc_zygote_magic)
	fastc_zygote_job_u32(mut job, u32(objects.len))
	fastc_zygote_job_text(mut job, options)
	for object in objects {
		fastc_zygote_job_text(mut job, object)
	}
	return job
}

// fastc_write_zygote_head streams the declarations that every unit sees, with
// each definition that only one unit may hold as its `extern` declaration.
fn fastc_write_zygote_head(fd int, pieces []string, units &FastcUnitLayout) bool {
	mut out := strings.new_builder(fastc_unit_stdin_buffer_size)
	defer {
		C.close(fd)
		// Only this worker owns the buffer; all writes completed before it is freed.
		unsafe { out.free() }
	}
	for k in 0 .. units.head_end {
		mut piece := pieces[k]
		for e, index in units.extern_indexes {
			if index == k {
				piece = units.extern_texts[e]
				break
			}
		}
		if !fastc_append_c_unit_piece(fd, mut out, piece) {
			return false
		}
	}
	if !fastc_append_c_unit_piece(fd, mut out, fastc_zygote_marker) {
		return false
	}
	return fastc_flush_c_unit_stdin(fd, mut out)
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

fn fastc_compile_libtcc_zygote_c_units(mut zygote FastcZygote, base_args []string, objects []string, pieces []string, units FastcUnitLayout, plan FastcZygotePlan, mut prepared_link FastcPreparedLink, preload_link bool) ! {
	unit_count := objects.len
	if !zygote.active || unit_count < 2 || unit_count > zygote.unit_fds.len
		|| plan.unit_items.len != unit_count + 1 {
		return error('invalid FastC unit zygote layout')
	}
	bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
	sw := time.new_stopwatch()
	zygote.active = false
	job := fastc_zygote_job(fastc_libtcc_options(base_args), objects)
	job_written := fastc_write_stdin_bytes(zygote.control_fd, job.data, job.len)
	// The zygote parses the head while the unit writers fill their pipes.
	head_writer := spawn fastc_write_zygote_head(zygote.control_fd, pieces, &units)
	mut unit_writers := []thread bool{cap: unit_count}
	for g in 0 .. unit_count {
		unit_writers << spawn fastc_write_zygote_unit(zygote.unit_fds[g], pieces, &units, &plan, g)
	}
	for g in unit_count .. zygote.unit_fds.len {
		C.close(zygote.unit_fds[g])
	}
	// The diagnostics can exceed a pipe buffer, so they are drained while the
	// finished units are collected.
	diagnostics_reader := spawn fastc_read_zygote_diagnostics(zygote.diagnostics_fd)
	if bench_phases {
		mut sizes := []int{cap: unit_count}
		solo_items := plan.solo_pieces.len - 1
		for g in 0 .. unit_count {
			mut size := 0
			for item in plan.unit_items[g] .. plan.unit_items[g + 1] {
				if item < solo_items {
					for k in plan.solo_pieces[item] .. plan.solo_pieces[item + 1] {
						size += pieces[k].len
					}
				} else {
					for k in units.unit_starts[item - solo_items] .. units.unit_starts[item - solo_items + 1] {
						size += pieces[k].len
					}
				}
			}
			sizes << size / 1024
		}
		eprintln('fastc-phase tcc.units_started ${sw.elapsed().microseconds()}us zygote=${unit_count} kb=${sizes}')
	}
	mut done := []bool{len: unit_count}
	mut next_input := 0
	mut link_failure := ''
	mut finished := []u8{len: 64}
	for {
		got := int(C.read(zygote.done_fd, finished.data, uint(finished.len)))
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
	C.close(zygote.done_fd)
	mut written := head_writer.wait() && job_written
	for unit_writer in unit_writers {
		if !unit_writer.wait() {
			written = false
		}
	}
	code := C.v_fastc_zygote_wait(zygote.pid)
	diagnostics := diagnostics_reader.wait()
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
