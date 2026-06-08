// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.transformer
import runtime
import os
import time

$if !windows {
	struct TransformChunkArgs {
		t                voidptr // &transformer.Transformer
		flat             &ast.FlatAst = unsafe { nil }
		flat_extra_stmts [][]ast.Stmt
		flat_start       int
		flat_end         int
		result_ptr       voidptr
		worker_ptr       voidptr
		worker_idx       int
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
		// Streaming rehydration: rehydrate one file at a time, transform it,
		// then drop the legacy form. Under GC, peak per worker is one file's
		// legacy AST instead of the whole chunk.
		mut result := []ast.File{cap: a.flat_end - a.flat_start}
		for fi := a.flat_start; fi < a.flat_end; fi++ {
			file := prepared_flat_file_for_parallel_transform(a.flat, a.flat_extra_stmts, fi) or {
				continue
			}
			result << w.transform_file_pub(file)
		}
		unsafe {
			*(&[]ast.File(a.result_ptr)) = result
			*(&voidptr(a.worker_ptr)) = voidptr(w)
		}
		return unsafe { nil }
	}

	// TransformChunkFlatArgs is the flat-direct counterpart of
	// TransformChunkArgs: the worker emits its file range cursor-native into its
	// own FlatBuilder (no legacy ast.File rehydrate) and hands back the resulting
	// FlatAst, which the main thread merges via `FlatBuilder.append_flat`.
	struct TransformChunkFlatArgs {
		t                voidptr // &transformer.Transformer
		flat             &ast.FlatAst = unsafe { nil }
		flat_extra_stmts [][]ast.Stmt
		flat_start       int
		flat_end         int
		result_ptr       voidptr // &ast.FlatAst
		worker_ptr       voidptr
		worker_idx       int
	}

	fn transform_chunk_flat_thread(arg voidptr) voidptr {
		a := unsafe { &TransformChunkFlatArgs(arg) }
		t := unsafe { &transformer.Transformer(a.t) }
		mut w := t.new_worker_clone(a.worker_idx)
		// Cursor-native: transform one file at a time straight into this
		// worker's FlatBuilder. Peak per worker is the cumulative flat for its
		// chunk (no legacy ast.File ever materialises).
		mut wb := ast.new_flat_builder()
		for fi := a.flat_start; fi < a.flat_end; fi++ {
			extra := if fi >= 0 && fi < a.flat_extra_stmts.len {
				a.flat_extra_stmts[fi]
			} else {
				[]ast.Stmt{}
			}
			w.transform_file_index_with_extra_to_flat(a.flat, fi, extra, mut wb)
		}
		unsafe {
			*(&ast.FlatAst(a.result_ptr)) = wb.flat
			*(&voidptr(a.worker_ptr)) = voidptr(w)
		}
		return unsafe { nil }
	}
}

fn flat_extra_stmts_by_file(extra_stmts map[int][]ast.Stmt, n_files int) [][]ast.Stmt {
	mut out := [][]ast.Stmt{cap: n_files}
	for _ in 0 .. n_files {
		out << []ast.Stmt{}
	}
	for fi, stmts in extra_stmts {
		if fi >= 0 && fi < n_files {
			out[fi] = stmts
		}
	}
	return out
}

fn prepared_flat_file_for_parallel_transform(flat &ast.FlatAst, flat_extra_stmts [][]ast.Stmt, fi int) ?ast.File {
	if fi < 0 || fi >= flat.files.len {
		return none
	}
	fc := flat.file_cursor(fi)
	stmt_list := fc.stmts()
	mut stmts := []ast.Stmt{cap: stmt_list.len()}
	for i in 0 .. stmt_list.len() {
		stmts << stmt_list.at(i).stmt()
	}
	if fi < flat_extra_stmts.len {
		stmts << flat_extra_stmts[fi]
	}
	return ast.File{
		name:           fc.name()
		mod:            fc.mod()
		selector_names: fc.selector_names()
		attributes:     fc.attrs().attributes()
		imports:        flat.read_file_imports(flat.files[fi])
		stmts:          stmts
	}
}

// transform_files_parallel_no_post_pass is the pre-post_pass portion of the
// parallel transform: pre_pass, fan-out across workers (or single-thread
// fallback), join, merge worker state, set synth_pos_counter. The transform
// always streams from the post-parse FlatAst (workers rehydrate one legacy
// ast.File at a time); `transform_files_parallel_to_flat_via_driver` runs
// `post_pass_to_flat` on the flattened output afterwards.
fn (mut b Builder) transform_files_parallel_no_post_pass(mut trans transformer.Transformer) []ast.File {
	// Pre-pass: sequential (builds elided_fns and runtime const inits)
	trans.pre_pass_from_flat(&b.flat)
	timing_impl := os.getenv('V2_TTIME') != ''
	mut sw_impl := time.new_stopwatch()
	mut flat_extra_stmts := [][]ast.Stmt{}
	if trans.needs_full_files_for_transform() {
		extra_stmts := trans.prepare_flat_for_transform(&b.flat)
		flat_extra_stmts = flat_extra_stmts_by_file(extra_stmts, b.flat.files.len)
	}
	if timing_impl {
		eprintln('  [ttime] prepare_files_for_transform total: ${sw_impl.elapsed().milliseconds()}ms')
		sw_impl = time.new_stopwatch()
	}
	defer {
		if timing_impl {
			eprintln('  [ttime] per-file fanout: ${sw_impl.elapsed().milliseconds()}ms')
		}
	}

	// Workers stream the rehydration per file (one legacy ast.File in flight
	// per worker at a time) directly from the post-parse FlatAst.
	n_jobs := runtime.nr_jobs()
	n_files := b.flat.files.len
	$if windows {
		mut result := []ast.File{cap: n_files}
		for fi in 0 .. n_files {
			file := prepared_flat_file_for_parallel_transform(&b.flat, flat_extra_stmts, fi) or {
				continue
			}
			result << trans.transform_file_pub(file)
		}
		return result
	} $else {
		if n_files <= 1 || n_jobs <= 1 {
			mut result := []ast.File{cap: n_files}
			for fi in 0 .. n_files {
				file := prepared_flat_file_for_parallel_transform(&b.flat, flat_extra_stmts, fi) or {
					continue
				}
				result << trans.transform_file_pub(file)
			}
			return result
		}

		// Split the files into contiguous [start,end) ranges, one per worker.
		mut bucket_indices := [][]int{len: n_jobs}
		chunk_size := (n_files + n_jobs - 1) / n_jobs
		mut i := 0
		mut bw := 0
		for i < n_files {
			end := if i + chunk_size < n_files { i + chunk_size } else { n_files }
			for j in i .. end {
				bucket_indices[bw] << j
			}
			i = end
			bw++
		}

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
		for w in 0 .. n_jobs {
			idxs := bucket_indices[w]
			if idxs.len == 0 {
				continue
			}
			args << TransformChunkArgs{
				t:                unsafe { voidptr(trans) }
				flat:             unsafe { &b.flat }
				flat_extra_stmts: flat_extra_stmts
				flat_start:       idxs[0]
				flat_end:         idxs[idxs.len - 1] + 1
				result_ptr:       unsafe { voidptr(&chunk_results[chunk_idx]) }
				worker_ptr:       unsafe { voidptr(&worker_ptrs[chunk_idx]) }
				worker_idx:       chunk_idx
			}
			C.pthread_create(unsafe { &thread_ids[chunk_idx] }, attr, transform_chunk_thread,
				unsafe { voidptr(&args[chunk_idx]) })
			chunk_idx++
		}
		C.pthread_attr_destroy(attr)

		// Wait for all workers
		for ci := 0; ci < chunk_idx; ci++ {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}

		// Scatter each worker's results back to original file order and merge
		// accumulated state. bucket_indices[w] lists the original indices the
		// w-th spawned worker processed, in the same order it produced results.
		mut result := []ast.File{len: n_files}
		mut ci := 0
		for w in 0 .. n_jobs {
			idxs := bucket_indices[w]
			if idxs.len == 0 {
				continue
			}
			chunk_files := chunk_results[ci]
			for k, fi in idxs {
				if k < chunk_files.len {
					result[fi] = chunk_files[k]
				}
			}
			worker := unsafe { &transformer.Transformer(worker_ptrs[ci]) }
			trans.merge_worker(worker)
			ci++
		}
		// Set synth_pos_counter past all worker ranges to avoid ID collisions in post_pass.
		trans.set_synth_pos_counter(-(chunk_idx * 100_000) - 1)
		return result
	}
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
// `keep_files` controls whether the transformed `[]ast.File` is materialized
// for the caller. Flat-codegen backends (cleanc/c/x64/arm64) drop `b.files`
// after transform, so they pass `keep_files = false`: the function then mirrors
// the flat-direct tail (`post_pass_to_flat` + `apply_post_pass_tail_from_flat`,
// no legacy file post-pass, whose edits are already on the flat) and frees the
// transient `result` here instead of threading it back only to be dropped.
// The `.v`/eval backends still consume the files, so they pass `keep_files = true`.
// transform_files_parallel_flat_direct is the flat-native parallel transform
// for flat-codegen backends. It mirrors the sequential
// `transform_flat_to_flat_direct` (pre_pass -> prepare -> per-file cursor
// transform -> post_pass tail) but fans the per-file loop across worker threads:
// each worker emits its contiguous file range cursor-native into its OWN
// FlatBuilder, then the main thread concatenates them in file order via
// `FlatBuilder.append_flat`. This replaces the legacy per-worker `ast.File`
// rehydrate (`transform_file_pub`) + main-thread `append_file` flatten, so no
// legacy AST is materialised on the default cleanc/c/x64 build path.
//
// Per-worker synth-position disjointness is handled by `new_worker_clone`
// (it offsets synth_pos_counter by `-worker_idx * 100_000`); worker state is
// merged back via `merge_worker` after the join, exactly as the legacy path.
fn (mut b Builder) transform_files_parallel_flat_direct(mut trans transformer.Transformer) ast.FlatAst {
	// Pre-pass + generic preparation: sequential, once (same as the legacy path).
	trans.pre_pass_from_flat(&b.flat)
	mut flat_extra_stmts := [][]ast.Stmt{}
	if trans.needs_full_files_for_transform() {
		extra_stmts := trans.prepare_flat_for_transform(&b.flat)
		flat_extra_stmts = flat_extra_stmts_by_file(extra_stmts, b.flat.files.len)
	}

	n_jobs := runtime.nr_jobs()
	n_files := b.flat.files.len
	mut out := ast.new_flat_builder()
	$if windows {
		for fi in 0 .. n_files {
			extra := if fi < flat_extra_stmts.len { flat_extra_stmts[fi] } else { []ast.Stmt{} }
			trans.transform_file_index_with_extra_to_flat(&b.flat, fi, extra, mut out)
		}
	} $else {
		if n_files <= 1 || n_jobs <= 1 {
			for fi in 0 .. n_files {
				extra := if fi < flat_extra_stmts.len { flat_extra_stmts[fi] } else { []ast.Stmt{} }
				trans.transform_file_index_with_extra_to_flat(&b.flat, fi, extra, mut out)
			}
		} else {
			// Contiguous [start,end) file ranges, one per worker. Merging the
			// worker outputs in spawn order preserves original file order.
			mut bucket_indices := [][]int{len: n_jobs}
			chunk_size := (n_files + n_jobs - 1) / n_jobs
			mut i := 0
			mut bw := 0
			for i < n_files {
				end := if i + chunk_size < n_files { i + chunk_size } else { n_files }
				for j in i .. end {
					bucket_indices[bw] << j
				}
				i = end
				bw++
			}

			mut chunk_flats := []ast.FlatAst{len: n_jobs}
			mut worker_ptrs := []voidptr{len: n_jobs, init: unsafe { nil }}
			mut thread_ids := []C.pthread_t{len: n_jobs}
			mut args := []TransformChunkFlatArgs{cap: n_jobs}

			// 64 MB worker stacks: ARM64-compiled transform recursion is stack-heavy.
			attr_buf := [64]u8{}
			attr := unsafe { voidptr(&attr_buf[0]) }
			C.pthread_attr_init(attr)
			C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)

			mut chunk_idx := 0
			for w in 0 .. n_jobs {
				idxs := bucket_indices[w]
				if idxs.len == 0 {
					continue
				}
				args << TransformChunkFlatArgs{
					t:                unsafe { voidptr(trans) }
					flat:             unsafe { &b.flat }
					flat_extra_stmts: flat_extra_stmts
					flat_start:       idxs[0]
					flat_end:         idxs[idxs.len - 1] + 1
					result_ptr:       unsafe { voidptr(&chunk_flats[chunk_idx]) }
					worker_ptr:       unsafe { voidptr(&worker_ptrs[chunk_idx]) }
					worker_idx:       chunk_idx
				}
				C.pthread_create(unsafe { &thread_ids[chunk_idx] }, attr,
					transform_chunk_flat_thread, unsafe { voidptr(&args[chunk_idx]) })
				chunk_idx++
			}
			C.pthread_attr_destroy(attr)
			for ci := 0; ci < chunk_idx; ci++ {
				C.pthread_join(thread_ids[ci], unsafe { nil })
			}

			// Concatenate worker flats in spawn (= file) order, merging worker state.
			mut ci := 0
			for w in 0 .. n_jobs {
				if bucket_indices[w].len == 0 {
					continue
				}
				out.append_flat(chunk_flats[ci])
				worker := unsafe { &transformer.Transformer(worker_ptrs[ci]) }
				trans.merge_worker(worker)
				ci++
			}
			// Move synth_pos_counter past all worker ranges for the post_pass.
			trans.set_synth_pos_counter(-(chunk_idx * 100_000) - 1)
		}
	}

	// Post-pass tail: identical to transform_flat_to_flat_direct's tail.
	generated_parts := trans.generated_fns_parts_from_flat(&out.flat)
	trans.post_pass_to_flat(mut out, generated_parts)
	trans.apply_post_pass_tail_from_flat(&out.flat)
	return out.flat
}

fn (mut b Builder) transform_files_parallel_to_flat_via_driver(mut trans transformer.Transformer, keep_files bool) (ast.FlatAst, []ast.File) {
	if !keep_files {
		// Flat-codegen backends (cleanc/c/x64/arm64): workers transform
		// cursor-native into per-worker FlatBuilders, merged via append_flat —
		// no legacy ast.File ever materialises. This is the flat-native parallel
		// transform that the merge primitive (FlatBuilder.append_flat) unlocked.
		return b.transform_files_parallel_flat_direct(mut trans), []ast.File{}
	}
	// `.v`/eval backends still consume the transformed []ast.File: keep the
	// legacy per-worker rehydrate-then-flatten path below.
	mut result := b.transform_files_parallel_no_post_pass(mut trans)
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
	if !keep_files {
		// Same tail as `transform_flat_to_flat_direct`: the flat already
		// received the file-mutating post-pass via `post_pass_to_flat`, so the
		// legacy `post_pass_files_with_generated_parts` is redundant. Run the
		// type-propagation tail against the flat and let `result` be freed.
		trans.apply_post_pass_tail_from_flat(&builder.flat)
		return builder.flat, []ast.File{}
	}
	trans.post_pass_files_with_generated_parts(mut result, generated_parts)
	// The compatibility files now receive the same file-mutating post-pass
	// edits as the flat output, so run the non-file tail on those files for
	// downstream legacy consumers.
	trans.apply_post_pass_tail(result)
	return builder.flat, result
}
