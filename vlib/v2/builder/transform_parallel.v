// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.transformer
import runtime

struct TransformChunkArgs {
	t          voidptr // &transformer.Transformer
	files      []ast.File
	result_ptr voidptr
	worker_ptr voidptr
	worker_idx int
}

fn C.pthread_create(thread voidptr, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.pthread_join(thread voidptr, retval voidptr) int

fn transform_chunk_thread(arg voidptr) voidptr {
	a := unsafe { &TransformChunkArgs(arg) }
	t := unsafe { &transformer.Transformer(a.t) }
	mut w := t.new_worker_clone(a.worker_idx)
	mut result := []ast.File{cap: a.files.len}
	for i := 0; i < a.files.len; i++ {
		result << w.transform_file_pub(a.files[i])
	}
	unsafe {
		*(&[]ast.File(a.result_ptr)) = result
		*(&voidptr(a.worker_ptr)) = voidptr(w)
	}
	return unsafe { nil }
}

fn (mut b Builder) transform_files_parallel(mut trans transformer.Transformer) []ast.File {
	// Pre-pass: sequential (builds elided_fns and runtime const inits)
	trans.pre_pass(b.files)

	// Per-file transformation: parallel
	n_jobs := runtime.nr_jobs()
	n_files := b.files.len
	if n_files <= 1 || n_jobs <= 1 {
		mut result := []ast.File{cap: n_files}
		for i := 0; i < n_files; i++ {
			result << trans.transform_file_pub(b.files[i])
		}
		trans.post_pass(mut result)
		return result
	}

	// Split files into chunks and spawn workers via pthreads
	chunk_size := (n_files + n_jobs - 1) / n_jobs // ceiling division
	mut chunk_results := [][]ast.File{len: n_jobs}
	mut worker_ptrs := []voidptr{len: n_jobs, init: unsafe { nil }}
	mut thread_ids := []voidptr{len: n_jobs, init: unsafe { nil }}
	mut args := []TransformChunkArgs{cap: n_jobs}
	mut chunk_idx := 0
	mut i := 0
	for i < n_files {
		end := if i + chunk_size < n_files { i + chunk_size } else { n_files }
		chunk := b.files[i..end]
		args << TransformChunkArgs{
			t:          unsafe { voidptr(&trans) }
			files:      chunk
			result_ptr: unsafe { voidptr(&chunk_results[chunk_idx]) }
			worker_ptr: unsafe { voidptr(&worker_ptrs[chunk_idx]) }
			worker_idx: chunk_idx
		}
		C.pthread_create(unsafe { voidptr(&thread_ids[chunk_idx]) }, unsafe { nil }, transform_chunk_thread,
			unsafe { voidptr(&args[chunk_idx]) })
		i = end
		chunk_idx++
	}

	// Wait for all workers
	for ci := 0; ci < chunk_idx; ci++ {
		C.pthread_join(thread_ids[ci], unsafe { nil })
	}

	// Collect results in chunk order and merge worker accumulated state
	mut result := []ast.File{cap: n_files}
	for ci := 0; ci < chunk_idx; ci++ {
		chunk_files := chunk_results[ci]
		for k := 0; k < chunk_files.len; k++ {
			result << chunk_files[k]
		}
		w := unsafe { &transformer.Transformer(worker_ptrs[ci]) }
		trans.merge_worker(w)
	}
	// Set synth_pos_counter past all worker ranges to avoid ID collisions in post_pass.
	trans.set_synth_pos_counter(-(chunk_idx * 100_000) - 1)

	// Post-pass: sequential
	trans.post_pass(mut result)
	return result
}
