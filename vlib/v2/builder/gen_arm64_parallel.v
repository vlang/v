// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import runtime
import v2.gen.arm64

struct GenARM64ChunkArgs {
	gen       voidptr // &arm64.Gen (main gen, used as template for cloning)
	start_idx int
	end_idx   int
	worker    voidptr // &voidptr — output slot for worker Gen pointer
}

fn C.pthread_create(thread voidptr, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int

fn C.pthread_join(thread voidptr, retval voidptr) int

fn gen_arm64_chunk_thread(arg voidptr) voidptr {
	a := unsafe { &GenARM64ChunkArgs(arg) }
	g := unsafe { &arm64.Gen(a.gen) }
	mut w := g.new_worker_clone()
	for fi := a.start_idx; fi < a.end_idx; fi++ {
		w.gen_func(g.mod.funcs[fi])
	}
	unsafe {
		*(&voidptr(a.worker)) = voidptr(w)
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

	// Split functions into chunks and spawn workers via pthreads
	chunk_size := (n_funcs + n_jobs - 1) / n_jobs
	mut worker_ptrs := []voidptr{len: n_jobs, init: unsafe { nil }}
	mut thread_ids := []voidptr{len: n_jobs, init: unsafe { nil }}
	mut args := []GenARM64ChunkArgs{cap: n_jobs}
	mut chunk_idx := 0
	mut i := 0
	for i < n_funcs {
		end := if i + chunk_size < n_funcs { i + chunk_size } else { n_funcs }
		args << GenARM64ChunkArgs{
			gen:       unsafe { voidptr(&gen) }
			start_idx: i
			end_idx:   end
			worker:    unsafe { voidptr(&worker_ptrs[chunk_idx]) }
		}
		C.pthread_create(unsafe { voidptr(&thread_ids[chunk_idx]) }, unsafe { nil },
			gen_arm64_chunk_thread, unsafe { voidptr(&args[chunk_idx]) })
		i = end
		chunk_idx++
	}

	// Wait for all workers
	for ci := 0; ci < chunk_idx; ci++ {
		C.pthread_join(thread_ids[ci], unsafe { nil })
	}

	// Merge worker results in order
	for ci := 0; ci < chunk_idx; ci++ {
		w := unsafe { &arm64.Gen(worker_ptrs[ci]) }
		gen.merge_worker(w)
	}

	gen.gen_post_pass()
}
