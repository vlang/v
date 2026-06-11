// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.transformer
import runtime

$if !windows {
	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int
	fn C.pthread_attr_init(attr voidptr) int
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int
	fn C.pthread_attr_destroy(attr voidptr) int

	// TransformChunkFlatArgs carries one worker's slice of the parallel flat
	// transform: the worker emits its file range cursor-native into its own
	// FlatBuilder (no legacy ast.File rehydrate) and hands back the resulting
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

// transform_files_parallel_flat_direct is the flat-native parallel transform
// used by every backend. It mirrors the sequential
// `transform_flat_to_flat_direct` (pre_pass -> prepare -> per-file cursor
// transform -> post_pass tail) but fans the per-file loop across worker threads:
// each worker emits its contiguous file range cursor-native into its OWN
// FlatBuilder, then the main thread concatenates them in file order via
// `FlatBuilder.append_flat`. No legacy ast.File is materialised; backends that
// still consume []ast.File (.v/eval) rehydrate from the transformed flat at
// the codegen boundary in builder.v.
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
