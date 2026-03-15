// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import runtime
import v2.ast
import v2.ssa

// FnDeclRef references a function declaration within the files array.
struct FnDeclRef {
	file_idx int
	stmt_idx int
	mod_name string
}

struct SSABuildChunkArgs {
	worker    voidptr // &ssa.Builder (pre-created worker builder)
	files     voidptr // &[]ast.File
	fn_refs   voidptr // &[]FnDeclRef
	start_idx int
	end_idx   int
}

fn C.pthread_create(thread voidptr, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
fn C.pthread_join(thread voidptr, retval voidptr) int
fn C.pthread_attr_init(attr voidptr) int
fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int
fn C.pthread_attr_destroy(attr voidptr) int

fn ssa_build_chunk_thread(arg voidptr) voidptr {
	a := unsafe { &SSABuildChunkArgs(arg) }
	mut worker_b := unsafe { &ssa.Builder(a.worker) }
	files := unsafe { &[]ast.File(a.files) }
	fn_refs := unsafe { &[]FnDeclRef(a.fn_refs) }

	// Build assigned functions.
	// Avoid chained array access like files[i].stmts[j] — in ARM64-compiled
	// binaries, chained indexing returns copies with potentially corrupted fields.
	// Instead, copy to a local first, then access fields.
	for fi := a.start_idx; fi < a.end_idx; fi++ {
		ref := unsafe { fn_refs[fi] }
		worker_b.cur_module = ref.mod_name
		file := unsafe { (*files)[ref.file_idx] }
		stmt := file.stmts[ref.stmt_idx]
		decl := stmt as ast.FnDecl
		worker_b.build_fn(decl)
	}
	return unsafe { nil }
}

fn (mut b Builder) ssa_build_parallel(mut ssa_builder ssa.Builder, files []ast.File) {
	n_jobs := runtime.nr_jobs()
	mut mod := ssa_builder.mod

	// Collect all function declarations that need building.
	// Avoid chained array access like files[fi].stmts[si] — copy to locals first.
	has_markused := ssa_builder.used_fn_keys.len > 0
	mut fn_refs := []FnDeclRef{cap: 4096}
	for fi in 0 .. files.len {
		file := files[fi]
		mod_name := ssa.file_module_name(file)
		nstmts := file.stmts.len
		for si in 0 .. nstmts {
			stmt := file.stmts[si]
			if stmt is ast.FnDecl {
				decl := stmt as ast.FnDecl
				if decl.language == .c && decl.stmts.len == 0 {
					continue
				}
				if decl.typ.generic_params.len > 0 {
					continue
				}
				// Dead code elimination: skip unreachable functions
				if has_markused {
					ssa_builder.cur_module = mod_name
					if !ssa_builder.should_build_fn(file.name, decl) {
						continue
					}
				}
				fn_refs << FnDeclRef{
					file_idx: fi
					stmt_idx: si
					mod_name: mod_name
				}
			}
		}
	}

	n_fns := fn_refs.len
	if n_fns <= 1 || n_jobs <= 1 {
		// Fallback to sequential
		ssa_builder.build_all_fn_bodies(files)
		return
	}

	// Pre-create all worker modules and builders on the main thread
	// to avoid COW races on shared data structures.
	chunk_size := (n_fns + n_jobs - 1) / n_jobs
	mut actual_chunks := 0
	mut i := 0
	for i < n_fns {
		actual_chunks++
		i += chunk_size
	}

	// Record seed lengths for merge — workers' new data starts beyond these.
	seed_values := mod.values.len
	seed_instrs := mod.instrs.len
	seed_blocks := mod.blocks.len
	seed_types := mod.type_store.types.len

	mut workers := []voidptr{cap: actual_chunks}
	for ci := 0; ci < actual_chunks; ci++ {
		mut worker_mod := mod.new_worker_module()
		mut worker_b := ssa_builder.new_worker_clone(worker_mod)
		workers << voidptr(worker_b)
	}

	// Spawn worker threads
	mut thread_ids := []voidptr{len: actual_chunks, init: unsafe { nil }}
	mut args := []SSABuildChunkArgs{cap: actual_chunks}

	attr_buf := [64]u8{}
	attr := unsafe { voidptr(&attr_buf[0]) }
	C.pthread_attr_init(attr)
	C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)

	mut chunk_idx := 0
	i = 0
	for i < n_fns {
		end := if i + chunk_size < n_fns { i + chunk_size } else { n_fns }
		args << SSABuildChunkArgs{
			worker:    workers[chunk_idx]
			files:     unsafe { voidptr(&files) }
			fn_refs:   unsafe { voidptr(&fn_refs) }
			start_idx: i
			end_idx:   end
		}
		C.pthread_create(unsafe { voidptr(&thread_ids[chunk_idx]) }, attr, ssa_build_chunk_thread,
			unsafe { voidptr(&args[chunk_idx]) })
		i = end
		chunk_idx++
	}
	C.pthread_attr_destroy(attr)

	// Wait for all workers
	for ci := 0; ci < chunk_idx; ci++ {
		C.pthread_join(thread_ids[ci], unsafe { nil })
	}

	// Merge worker results in order
	for ci := 0; ci < chunk_idx; ci++ {
		w := unsafe { &ssa.Builder(workers[ci]) }
		w_mod := w.mod
		// Collect func_data from worker's modified funcs[]
		mut func_data := []ssa.FuncSSAData{cap: 512}
		for fi2 := 0; fi2 < w_mod.funcs.len; fi2++ {
			wf := w_mod.funcs[fi2]
			if wf.blocks.len > 0 {
				func_data << ssa.FuncSSAData{
					func_idx: fi2
					blocks:   wf.blocks
					params:   wf.params
				}
			}
		}
		mod.merge_worker_module(w_mod, func_data, seed_values, seed_instrs, seed_blocks,
			seed_types)
	}
}
