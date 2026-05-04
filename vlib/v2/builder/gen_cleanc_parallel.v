// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import runtime
import time
import v2.gen.cleanc

struct GenCleancChunkArgs {
	worker           voidptr // &cleanc.Gen — pre-cloned worker
	file_indices_ptr voidptr // &[]int — file indices to process
}

@[typedef]
struct C.pthread_t {}

fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.pthread_join(thread C.pthread_t, retval voidptr) int
fn C.pthread_attr_init(attr voidptr) int
fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int
fn C.pthread_attr_destroy(attr voidptr) int

fn gen_cleanc_chunk_thread(arg voidptr) voidptr {
	a := unsafe { &GenCleancChunkArgs(arg) }
	mut w := unsafe { &cleanc.Gen(a.worker) }
	indices := unsafe { &[]int(a.file_indices_ptr) }
	w.gen_pass5_files(*indices)
	return unsafe { nil }
}

fn print_cleanc_parallel_step_time(stats_enabled bool, step string, elapsed time.Duration) {
	if !stats_enabled {
		return
	}
	println('   - C Gen/full ${step}: ${elapsed.milliseconds()}ms')
}

fn mark_cleanc_parallel_step(stats_enabled bool, mut sw time.StopWatch, stage_start time.Duration, step string) time.Duration {
	if !stats_enabled {
		return stage_start
	}
	now := sw.elapsed()
	print_cleanc_parallel_step_time(true, step, time.Duration(now - stage_start))
	return now
}

fn (mut b Builder) gen_cleanc_parallel(mut gen cleanc.Gen) {
	stats_enabled := b.pref != unsafe { nil } && b.pref.stats
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()
	emit_indices := gen.gen_pass5_pre()
	stage_start = mark_cleanc_parallel_step(stats_enabled, mut stats_sw, stage_start, 'pass 5 pre')

	n_files := emit_indices.len
	n_jobs := runtime.nr_jobs()

	if n_files <= 1 || n_jobs <= 1 {
		// Fallback to sequential
		gen.gen_pass5_files(emit_indices)
		stage_start = mark_cleanc_parallel_step(stats_enabled, mut stats_sw, stage_start,
			'pass 5 files')
		gen.gen_pass5_post()
		_ = mark_cleanc_parallel_step(stats_enabled, mut stats_sw, stage_start, 'pass 5 post')
		return
	}

	// Split files into chunks using round-robin interleaving for balanced load.
	// This avoids one worker getting all the heavy files (e.g., json2/decode.v, ui/window.v).
	chunk_size := (n_files + n_jobs - 1) / n_jobs
	mut thread_ids := []C.pthread_t{len: n_jobs}
	mut args := []GenCleancChunkArgs{cap: n_jobs}
	mut workers := []voidptr{cap: n_jobs}
	mut chunk_indices := [][]int{cap: n_jobs}

	mut chunk_idx := n_jobs
	if chunk_idx > n_files {
		chunk_idx = n_files
	}
	for ci := 0; ci < chunk_idx; ci++ {
		chunk_indices << []int{cap: chunk_size}
	}
	for i := 0; i < n_files; i++ {
		chunk_indices[i % chunk_idx] << emit_indices[i]
	}
	stage_start = mark_cleanc_parallel_step(stats_enabled, mut stats_sw, stage_start,
		'pass 5 chunk split')
	for ci := 0; ci < chunk_idx; ci++ {
		w := gen.new_pass5_worker(chunk_indices[ci], ci)
		workers << voidptr(w)
	}
	stage_start = mark_cleanc_parallel_step(stats_enabled, mut stats_sw, stage_start,
		'pass 5 worker setup')

	// Set up args after all chunk_indices are stable
	for ci := 0; ci < chunk_idx; ci++ {
		args << GenCleancChunkArgs{
			worker:           workers[ci]
			file_indices_ptr: unsafe { voidptr(&chunk_indices[ci]) }
		}
	}

	attr_buf := [64]u8{}
	attr := unsafe { voidptr(&attr_buf[0]) }
	C.pthread_attr_init(attr)
	C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)

	for ci := 0; ci < chunk_idx; ci++ {
		C.pthread_create(unsafe { &thread_ids[ci] }, attr, gen_cleanc_chunk_thread,
			unsafe { voidptr(&args[ci]) })
	}
	C.pthread_attr_destroy(attr)

	// Wait for all workers
	for ci := 0; ci < chunk_idx; ci++ {
		C.pthread_join(thread_ids[ci], unsafe { nil })
	}
	stage_start = mark_cleanc_parallel_step(stats_enabled, mut stats_sw, stage_start,
		'pass 5 worker run')

	// Merge worker results in order
	for ci := 0; ci < chunk_idx; ci++ {
		w := unsafe { &cleanc.Gen(workers[ci]) }
		gen.merge_pass5_worker(w)
	}
	stage_start = mark_cleanc_parallel_step(stats_enabled, mut stats_sw, stage_start,
		'pass 5 merge')

	gen.gen_pass5_post()
	_ = mark_cleanc_parallel_step(stats_enabled, mut stats_sw, stage_start, 'pass 5 post')
}
