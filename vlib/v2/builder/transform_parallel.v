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
	flat       &ast.FlatAst = unsafe { nil }
	flat_start int
	flat_end   int
	result_ptr voidptr
	worker_ptr voidptr
	worker_idx int
}

fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.pthread_join(thread C.pthread_t, retval voidptr) int
fn C.pthread_attr_init(attr voidptr) int
fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int
fn C.pthread_attr_destroy(attr voidptr) int

fn transform_chunk_thread(arg voidptr) voidptr {
	a := unsafe { &TransformChunkArgs(arg) }
	t := unsafe { &transformer.Transformer(a.t) }
	mut w := t.new_worker_clone(a.worker_idx)
	if unsafe { a.flat != nil } {
		// Streaming rehydration: rehydrate one file at a time, transform it,
		// then drop the legacy form. Under GC, peak per worker is one file's
		// legacy AST instead of the whole chunk.
		n := a.flat_end - a.flat_start
		mut result := []ast.File{cap: n}
		for fi := a.flat_start; fi < a.flat_end; fi++ {
			one := a.flat.to_files_range(fi, fi + 1)
			if one.len == 0 {
				continue
			}
			result << w.transform_file_pub(one[0])
		}
		unsafe {
			*(&[]ast.File(a.result_ptr)) = result
			*(&voidptr(a.worker_ptr)) = voidptr(w)
		}
		return unsafe { nil }
	}
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
	mut result := b.transform_files_parallel_no_post_pass(mut trans)
	trans.post_pass(mut result)
	return result
}

fn (mut b Builder) transform_files_parallel_from_flat(mut trans transformer.Transformer) []ast.File {
	mut result := b.transform_files_parallel_no_post_pass_from_flat(mut trans)
	trans.post_pass(mut result)
	return result
}

// transform_files_parallel_no_post_pass is the pre-post_pass portion of the
// parallel transform: pre_pass, fan-out across workers (or single-thread
// fallback), join, merge worker state, set synth_pos_counter. Shared by
// `transform_files_parallel` (which calls legacy `post_pass` after) and
// `transform_files_parallel_to_flat_via_driver` (which calls
// `post_pass_to_flat` on the flattened output instead).
fn (mut b Builder) transform_files_parallel_no_post_pass(mut trans transformer.Transformer) []ast.File {
	return b.transform_files_parallel_no_post_pass_impl(mut trans, b.flat_check_enabled)
}

fn (mut b Builder) transform_files_parallel_no_post_pass_from_flat(mut trans transformer.Transformer) []ast.File {
	return b.transform_files_parallel_no_post_pass_impl(mut trans, true)
}

fn (mut b Builder) transform_files_parallel_no_post_pass_impl(mut trans transformer.Transformer, stream_from_flat bool) []ast.File {
	// Pre-pass: sequential (builds elided_fns and runtime const inits)
	if stream_from_flat {
		trans.pre_pass_from_flat(&b.flat)
	} else {
		trans.pre_pass(b.files)
	}

	// In flat mode, workers stream the rehydration per file (one legacy
	// ast.File in flight per worker at a time). Otherwise b.files is the
	// canonical legacy-AST input — slice it across workers as before.
	n_jobs := runtime.nr_jobs()
	n_files := if stream_from_flat { b.flat.files.len } else { b.files.len }
	if n_files <= 1 || n_jobs <= 1 {
		mut result := []ast.File{cap: n_files}
		if stream_from_flat {
			for fi in 0 .. n_files {
				one := b.flat.to_files_range(fi, fi + 1)
				if one.len == 0 {
					continue
				}
				result << trans.transform_file_pub(one[0])
			}
		} else {
			for i := 0; i < n_files; i++ {
				result << trans.transform_file_pub(b.files[i])
			}
		}
		return result
	}

	// Split files into chunks and spawn workers via pthreads
	chunk_size := (n_files + n_jobs - 1) / n_jobs // ceiling division
	mut chunk_results := [][]ast.File{len: n_jobs}
	mut worker_ptrs := []voidptr{len: n_jobs, init: unsafe { nil }}
	mut thread_ids := []C.pthread_t{len: n_jobs}
	mut args := []TransformChunkArgs{cap: n_jobs}

	// ARM64-compiled code uses much more stack per function (one slot per SSA
	// value, no reuse). Increase worker thread stack size to 64 MB so deeply
	// recursive transform functions don't overflow the default 512 KB stack.
	attr_buf := [64]u8{}
	attr := unsafe { voidptr(&attr_buf[0]) }
	C.pthread_attr_init(attr)
	C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)

	mut chunk_idx := 0
	mut i := 0
	for i < n_files {
		end := if i + chunk_size < n_files { i + chunk_size } else { n_files }
		if stream_from_flat {
			args << TransformChunkArgs{
				t:          unsafe { voidptr(trans) }
				flat:       unsafe { &b.flat }
				flat_start: i
				flat_end:   end
				result_ptr: unsafe { voidptr(&chunk_results[chunk_idx]) }
				worker_ptr: unsafe { voidptr(&worker_ptrs[chunk_idx]) }
				worker_idx: chunk_idx
			}
		} else {
			chunk := b.files[i..end]
			args << TransformChunkArgs{
				t:          unsafe { voidptr(trans) }
				files:      chunk
				result_ptr: unsafe { voidptr(&chunk_results[chunk_idx]) }
				worker_ptr: unsafe { voidptr(&worker_ptrs[chunk_idx]) }
				worker_idx: chunk_idx
			}
		}
		C.pthread_create(unsafe { &thread_ids[chunk_idx] }, attr, transform_chunk_thread,
			unsafe { voidptr(&args[chunk_idx]) })
		i = end
		chunk_idx++
	}
	C.pthread_attr_destroy(attr)

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
	return result
}

// transform_files_parallel_to_flat is the parallel counterpart of
// Transformer.transform_files_to_flat. Today it composes the existing
// parallel transform with a boundary flatten_files() — same total work
// as before, just shifted from builder.v into the parallel path. The
// API shape mirrors the sequential wedge so the builder can route
// V2_MARKUSED_FLAT through a single uniform call site.
//
// The eventual perf win comes when each worker writes into a per-worker
// FlatBuilder instead of materialising legacy []ast.File chunks; this
// wedge is the right place to plug that in.
fn (mut b Builder) transform_files_parallel_to_flat(mut trans transformer.Transformer) (ast.FlatAst, []ast.File) {
	result := b.transform_files_parallel(mut trans)
	return ast.flatten_files(result), result
}

// transform_files_parallel_to_flat_via_driver is the parallel counterpart to
// `Transformer.transform_files_to_flat_via_driver` (s162). Same external
// shape (returns `(ast.FlatAst, []ast.File)`) but uses the s161 driver
// instead of the `legacy post_pass + flatten_files` boundary:
//
//   1. Per-worker transform via `transform_files_parallel_no_post_pass`
//      (skips legacy `post_pass`).
//   2. Compute `generated_fns_parts` against the un-post_pass'd result
//      (parts helper walks `[]ast.File` for module routing).
//   3. Flatten the un-post_pass'd files into a fresh FlatBuilder.
//   4. Run `post_pass_to_flat(mut builder, generated_parts)` — the s161
//      driver appends/prepends/replaces stmts on the flat directly.
//   5. Apply the non-file post_pass tail via `apply_post_pass_tail`.
//
// Bit-equivalent to legacy in tree structure (each file's stmts list holds
// the same content) but NOT bit-equal in full `signature()`: the
// `.file.extra` slot stores `intern(mod)` as a raw intern index, and the
// two paths intern strings in different orders. Compare via per-file
// `subtree_signature` of the stmts list (file root edge 2) to assert
// structural parity. Same property as s162's sequential wedge.
//
// Memory: zero saving over `transform_files_parallel_to_flat` while the SSA
// builder still consumes the returned `[]ast.File`. Once SSA migrates to
// flat, this entry can drop the `[]ast.File` return and the post-transform
// `[]ast.File` allocation disappears. Until then, this is migration
// scaffolding pinned by the same pattern as s162.
fn (mut b Builder) transform_files_parallel_to_flat_via_driver(mut trans transformer.Transformer) (ast.FlatAst, []ast.File) {
	result := b.transform_files_parallel_no_post_pass(mut trans)
	mut builder := ast.new_flat_builder()
	for file in result {
		builder.append_file(file)
	}
	// Compute parts against the freshly-appended flat (s166): no longer
	// needs `result []ast.File` for explicit_str / module routing — both
	// `explicit_str_method_fn_names_from_flat` (s165) and
	// `generated_fn_module_from_flat` (s164) walk `builder.flat` directly.
	generated_parts := trans.generated_fns_parts_from_flat(&builder.flat)
	trans.post_pass_to_flat(mut builder, generated_parts)
	// Tail runs on the post_pass'd flat (s167) — matches legacy semantics
	// where `post_pass(result)` mutates `result` BEFORE `propagate_types`
	// sees it. Pre-s167 wedges passed un-post_pass'd `result` here so
	// non-arm64 propagation saw stale stmts.
	trans.apply_post_pass_tail_from_flat(&builder.flat)
	return builder.flat, result
}
