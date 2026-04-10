// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import runtime
import v2.gen.arm64
import v2.mir

struct GenARM64ChunkArgs {
	worker    voidptr // &arm64.Gen — pre-cloned worker (created on main thread)
	mod_ptr   voidptr // &mir.Module — shared MIR module
	start_idx int
	end_idx   int
}

fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.pthread_join(thread C.pthread_t, retval voidptr) int
fn C.pthread_attr_init(attr voidptr) int
fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int
fn C.pthread_attr_destroy(attr voidptr) int

fn gen_arm64_chunk_thread(arg voidptr) voidptr {
	a := unsafe { &GenARM64ChunkArgs(arg) }
	mut w := unsafe { &arm64.Gen(a.worker) }
	m := unsafe { &mir.Module(a.mod_ptr) }
	for fi := a.start_idx; fi < a.end_idx; fi++ {
		w.gen_func(m.funcs[fi])
	}
	return unsafe { nil }
}

fn (mut b Builder) gen_arm64_parallel(mut gen arm64.Gen) {
	gen.gen_pre_pass()

	n_funcs := gen.mod.funcs.len
	n_jobs := runtime.nr_jobs()

	if n_funcs <= 1 || n_jobs <= 1 {
		// Fallback to sequential
		for fi := 0; fi < n_funcs; fi++ {
			gen.gen_func(gen.mod.funcs[fi])
		}
		gen.gen_post_pass()
		return
	}

	// Split functions into chunks
	chunk_size := (n_funcs + n_jobs - 1) / n_jobs
	mut thread_ids := []C.pthread_t{len: n_jobs}
	mut args := []GenARM64ChunkArgs{cap: n_jobs}

	// Pre-create all workers on the main thread to avoid concurrent .clone() races.
	// Each worker gets its own deep copy of maps/arrays.
	mut workers := []voidptr{cap: n_jobs}

	mut chunk_idx := 0
	mut i := 0
	for i < n_funcs {
		end := if i + chunk_size < n_funcs { i + chunk_size } else { n_funcs }
		w := gen.new_worker_clone()
		workers << voidptr(w)
		args << GenARM64ChunkArgs{
			worker:    voidptr(w)
			mod_ptr:   unsafe { voidptr(gen.mod) }
			start_idx: i
			end_idx:   end
		}
		i = end
		chunk_idx++
	}

	attr_buf := [64]u8{}
	attr := unsafe { voidptr(&attr_buf[0]) }
	C.pthread_attr_init(attr)
	C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)

	for ci := 0; ci < chunk_idx; ci++ {
		C.pthread_create(unsafe { &thread_ids[ci] }, attr, gen_arm64_chunk_thread, unsafe { voidptr(&args[ci]) })
	}
	C.pthread_attr_destroy(attr)

	// Wait for all workers
	for ci := 0; ci < chunk_idx; ci++ {
		C.pthread_join(thread_ids[ci], unsafe { nil })
	}

	// Merge worker results in order
	for ci := 0; ci < chunk_idx; ci++ {
		w := unsafe { &arm64.Gen(workers[ci]) }
		gen.merge_worker(w)
	}

	gen.gen_post_pass()
}
