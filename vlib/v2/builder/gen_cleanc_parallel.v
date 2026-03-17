// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import runtime
import v2.gen.cleanc

struct GenCleancChunkArgs {
	worker           voidptr // &cleanc.Gen — pre-cloned worker
	file_indices_ptr voidptr // &[]int — file indices to process
}

fn C.pthread_create(thread voidptr, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.pthread_join(thread voidptr, retval voidptr) int
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

fn (mut b Builder) gen_cleanc_parallel(mut gen cleanc.Gen) {
	emit_indices := gen.gen_pass5_pre()

	n_files := emit_indices.len
	n_jobs := runtime.nr_jobs()

	if n_files <= 1 || n_jobs <= 1 {
		// Fallback to sequential
		gen.gen_pass5_files(emit_indices)
		gen.gen_pass5_post()
		return
	}

	// Split files into chunks
	chunk_size := (n_files + n_jobs - 1) / n_jobs
	mut thread_ids := []voidptr{len: n_jobs, init: unsafe { nil }}
	mut args := []GenCleancChunkArgs{cap: n_jobs}
	mut workers := []voidptr{cap: n_jobs}
	mut chunk_indices := [][]int{cap: n_jobs}

	mut chunk_idx := 0
	mut i := 0
	for i < n_files {
		end := if i + chunk_size < n_files { i + chunk_size } else { n_files }
		mut indices := []int{cap: end - i}
		for j := i; j < end; j++ {
			indices << emit_indices[j]
		}
		chunk_indices << indices
		w := gen.new_pass5_worker(indices)
		workers << voidptr(w)
		i = end
		chunk_idx++
	}

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
		C.pthread_create(unsafe { voidptr(&thread_ids[ci]) }, attr, gen_cleanc_chunk_thread,
			unsafe { voidptr(&args[ci]) })
	}
	C.pthread_attr_destroy(attr)

	// Wait for all workers
	for ci := 0; ci < chunk_idx; ci++ {
		C.pthread_join(thread_ids[ci], unsafe { nil })
	}

	// Merge worker results in order
	for ci := 0; ci < chunk_idx; ci++ {
		w := unsafe { &cleanc.Gen(workers[ci]) }
		gen.merge_pass5_worker(w)
	}

	gen.gen_pass5_post()
}
