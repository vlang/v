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
		t          voidptr // &transformer.Transformer
		files      []ast.File
		flat       &ast.FlatAst = unsafe { nil }
		flat_start int
		flat_end   int
		result_ptr voidptr
		worker_ptr voidptr
		worker_idx int
	}

	struct TransformStmtJob {
		file_idx   int
		stmt_idx   int
		result_idx int
		cost       int
		whole_file bool
	}

	struct TransformStmtResult {
		result_idx int
		stmts      []ast.Stmt
		file       ast.File
		whole_file bool
	}

	struct TransformStmtChunkArgs {
		t          voidptr // &transformer.Transformer
		files      []ast.File
		jobs       []TransformStmtJob
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
		wprof := os.getenv('V2_TTIME') != ''
		file_prof := os.getenv('V2_TTIME_FILES') != ''
		mut wsw := time.new_stopwatch()
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
			mut fsw := time.new_stopwatch()
			result << w.transform_file_pub(a.files[i])
			if file_prof {
				eprintln('  [ttime-file] worker ${a.worker_idx}: ${a.files[i].name} ${fsw.elapsed().milliseconds()}ms')
			}
		}
		if wprof {
			eprintln('  [ttime] worker ${a.worker_idx}: ${a.files.len} files in ${wsw.elapsed().milliseconds()}ms')
		}
		unsafe {
			*(&[]ast.File(a.result_ptr)) = result
			*(&voidptr(a.worker_ptr)) = voidptr(w)
		}
		return unsafe { nil }
	}

	fn transform_stmt_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &TransformStmtChunkArgs(arg) }
		t := unsafe { &transformer.Transformer(a.t) }
		wprof := os.getenv('V2_TTIME') != ''
		file_prof := os.getenv('V2_TTIME_FILES') != ''
		mut wsw := time.new_stopwatch()
		mut w := t.new_worker_clone(a.worker_idx)
		mut result := []TransformStmtResult{cap: a.jobs.len}
		for job in a.jobs {
			file := a.files[job.file_idx]
			mut fsw := time.new_stopwatch()
			if job.whole_file {
				transformed := w.transform_file_pub(file)
				if file_prof {
					eprintln('  [ttime-file] worker ${a.worker_idx}: ${file.name} ${fsw.elapsed().milliseconds()}ms')
				}
				result << TransformStmtResult{
					result_idx: job.result_idx
					file:       transformed
					whole_file: true
				}
				continue
			}
			stmt := file.stmts[job.stmt_idx]
			stmts := w.transform_file_stmt_pub(file, stmt)
			if file_prof {
				eprintln('  [ttime-file] worker ${a.worker_idx}: ${file.name}:${job.stmt_idx} ${fsw.elapsed().milliseconds()}ms')
			}
			result << TransformStmtResult{
				result_idx: job.result_idx
				stmts:      stmts
			}
		}
		if wprof {
			eprintln('  [ttime] worker ${a.worker_idx}: ${a.jobs.len} stmts in ${wsw.elapsed().milliseconds()}ms')
		}
		unsafe {
			*(&[]TransformStmtResult(a.result_ptr)) = result
			*(&voidptr(a.worker_ptr)) = voidptr(w)
		}
		return unsafe { nil }
	}
}

fn (mut b Builder) transform_files_parallel(mut trans transformer.Transformer) []ast.File {
	timing := os.getenv('V2_TTIME') != ''
	mut sw := time.new_stopwatch()
	mut result := b.transform_files_parallel_no_post_pass(mut trans)
	if timing {
		eprintln('  [ttime] (parallel) prepare+fanout: ${sw.elapsed().milliseconds()}ms')
		sw = time.new_stopwatch()
	}
	trans.post_pass(mut result)
	if timing {
		eprintln('  [ttime] (parallel) post_pass: ${sw.elapsed().milliseconds()}ms')
	}
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
	timing_impl := os.getenv('V2_TTIME') != ''
	mut sw_impl := time.new_stopwatch()
	mut stream_files_from_flat := stream_from_flat
	mut files_to_transform := []ast.File{}
	if trans.needs_full_files_for_transform() {
		src_files := if stream_from_flat { b.flat.to_files() } else { b.files }
		files_to_transform = trans.prepare_files_for_transform(src_files)
		stream_files_from_flat = false
	} else if !stream_from_flat {
		files_to_transform = b.files.clone()
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

	// In flat mode, workers stream the rehydration per file (one legacy
	// ast.File in flight per worker at a time). Otherwise b.files is the
	// canonical legacy-AST input — slice it across workers as before.
	n_jobs := runtime.nr_jobs()
	n_files := if stream_files_from_flat { b.flat.files.len } else { files_to_transform.len }
	$if windows {
		mut result := []ast.File{cap: n_files}
		if stream_files_from_flat {
			for fi in 0 .. n_files {
				one := b.flat.to_files_range(fi, fi + 1)
				if one.len == 0 {
					continue
				}
				result << trans.transform_file_pub(one[0])
			}
		} else {
			for i := 0; i < n_files; i++ {
				result << trans.transform_file_pub(files_to_transform[i])
			}
		}
		return result
	} $else {
		if !stream_files_from_flat && n_jobs > 1 {
			return b.transform_files_parallel_top_level_stmts(mut trans, files_to_transform, n_jobs)
		}
		if n_files <= 1 || n_jobs <= 1 {
			mut result := []ast.File{cap: n_files}
			if stream_files_from_flat {
				for fi in 0 .. n_files {
					one := b.flat.to_files_range(fi, fi + 1)
					if one.len == 0 {
						continue
					}
					result << trans.transform_file_pub(one[0])
				}
			} else {
				for i := 0; i < n_files; i++ {
					result << trans.transform_file_pub(files_to_transform[i])
				}
			}
			return result
		}

		// Assign files to workers. Contiguous chunks badly unbalance the load:
		// the few huge files (transformer.v, monomorphize.v, the cleanc gen
		// files, ...) cluster into adjacent chunks, so 2-3 workers run ~10s
		// while the rest finish in <0.5s and idle. For the non-flat path we
		// instead use longest-processing-time-first (LPT) bucketing keyed on a
		// cheap size proxy, then scatter each worker's results back to their
		// original file index after the join (no concurrent writes — workers
		// each fill their own chunk_results slot, the merge happens serially).
		mut bucket_indices := [][]int{len: n_jobs}
		if stream_files_from_flat {
			// Flat streaming still uses contiguous [start,end) ranges.
			chunk_size := (n_files + n_jobs - 1) / n_jobs
			mut i := 0
			mut w := 0
			for i < n_files {
				end := if i + chunk_size < n_files { i + chunk_size } else { n_files }
				for j in i .. end {
					bucket_indices[w] << j
				}
				i = end
				w++
			}
		} else {
			bucket_indices = lpt_buckets(files_to_transform, n_jobs)
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
			if stream_files_from_flat {
				args << TransformChunkArgs{
					t:          unsafe { voidptr(trans) }
					flat:       unsafe { &b.flat }
					flat_start: idxs[0]
					flat_end:   idxs[idxs.len - 1] + 1
					result_ptr: unsafe { voidptr(&chunk_results[chunk_idx]) }
					worker_ptr: unsafe { voidptr(&worker_ptrs[chunk_idx]) }
					worker_idx: chunk_idx
				}
			} else {
				mut chunk := []ast.File{cap: idxs.len}
				for fi in idxs {
					chunk << files_to_transform[fi]
				}
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

fn (mut b Builder) transform_files_parallel_top_level_stmts(mut trans transformer.Transformer, files []ast.File, n_jobs int) []ast.File {
	$if windows {
		return files
	} $else {
		mut jobs := []TransformStmtJob{}
		mut stmt_job_indices := [][]int{cap: files.len}
		mut file_job_indices := []int{len: files.len, init: -1}
		for fi, file in files {
			mut indices := []int{len: file.stmts.len, init: -1}
			file_cost := file_transform_cost(file)
			if !file_can_split_top_level(file) || file_cost < 10000 {
				job_idx := jobs.len
				jobs << TransformStmtJob{
					file_idx:   fi
					result_idx: job_idx
					cost:       file_cost
					whole_file: true
				}
				file_job_indices[fi] = job_idx
			} else {
				for si, stmt in file.stmts {
					job_idx := jobs.len
					jobs << TransformStmtJob{
						file_idx:   fi
						stmt_idx:   si
						result_idx: job_idx
						cost:       top_level_transform_stmt_cost(stmt)
					}
					indices[si] = job_idx
				}
			}
			stmt_job_indices << indices
		}
		if jobs.len == 0 {
			return files
		}
		buckets := lpt_stmt_buckets(jobs, n_jobs)
		mut chunk_results := [][]TransformStmtResult{len: n_jobs}
		mut worker_ptrs := []voidptr{len: n_jobs, init: unsafe { nil }}
		mut thread_ids := []C.pthread_t{len: n_jobs}
		mut args := []TransformStmtChunkArgs{cap: n_jobs}

		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)

		mut chunk_idx := 0
		for w in 0 .. n_jobs {
			bucket := buckets[w]
			if bucket.len == 0 {
				continue
			}
			args << TransformStmtChunkArgs{
				t:          unsafe { voidptr(trans) }
				files:      files
				jobs:       bucket
				result_ptr: unsafe { voidptr(&chunk_results[chunk_idx]) }
				worker_ptr: unsafe { voidptr(&worker_ptrs[chunk_idx]) }
				worker_idx: chunk_idx
			}
			C.pthread_create(unsafe { &thread_ids[chunk_idx] }, attr, transform_stmt_chunk_thread,
				unsafe { voidptr(&args[chunk_idx]) })
			chunk_idx++
		}
		C.pthread_attr_destroy(attr)

		for ci := 0; ci < chunk_idx; ci++ {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}

		mut job_results := [][]ast.Stmt{len: jobs.len, init: []ast.Stmt{}}
		mut file_results := []ast.File{len: files.len}
		for ci := 0; ci < chunk_idx; ci++ {
			for item in chunk_results[ci] {
				if item.whole_file {
					job := jobs[item.result_idx]
					file_results[job.file_idx] = item.file
				} else {
					job_results[item.result_idx] = item.stmts
				}
			}
			worker := unsafe { &transformer.Transformer(worker_ptrs[ci]) }
			trans.merge_worker(worker)
		}

		mut result := []ast.File{cap: files.len}
		for fi, file in files {
			if file_job_indices[fi] >= 0 {
				result << file_results[fi]
				continue
			}
			mut stmts := []ast.Stmt{cap: file.stmts.len}
			indices := stmt_job_indices[fi]
			for si in 0 .. file.stmts.len {
				job_idx := indices[si]
				if job_idx >= 0 {
					stmts << job_results[job_idx]
				}
			}
			result << ast.File{
				attributes:     file.attributes
				mod:            file.mod
				name:           file.name
				stmts:          stmts
				imports:        file.imports
				selector_names: file.selector_names
			}
		}
		trans.set_synth_pos_counter(-(chunk_idx * 100_000) - 1)
		return result
	}
}

fn file_can_split_top_level(file ast.File) bool {
	for stmt in file.stmts {
		if !stmt_can_split_top_level(stmt) {
			return false
		}
	}
	return true
}

fn stmt_can_split_top_level(stmt ast.Stmt) bool {
	return match stmt {
		ast.ComptimeStmt, ast.ConstDecl, ast.Directive, ast.EmptyStmt, ast.EnumDecl, ast.FnDecl,
		ast.GlobalDecl, ast.ImportStmt, ast.InterfaceDecl, ast.ModuleStmt, ast.StructDecl,
		ast.TypeDecl, []ast.Attribute {
			true
		}
		else {
			false
		}
	}
}

fn lpt_stmt_buckets(jobs []TransformStmtJob, n_jobs int) [][]TransformStmtJob {
	mut order := []int{len: jobs.len, init: index}
	for i in 1 .. order.len {
		key := order[i]
		kc := jobs[key].cost
		mut j := i - 1
		for j >= 0 && jobs[order[j]].cost < kc {
			order[j + 1] = order[j]
			j--
		}
		order[j + 1] = key
	}
	mut buckets := [][]TransformStmtJob{len: n_jobs}
	mut load := []i64{len: n_jobs}
	for job_idx in order {
		mut mw := 0
		for w in 1 .. n_jobs {
			if load[w] < load[mw] {
				mw = w
			}
		}
		job := jobs[job_idx]
		buckets[mw] << job
		load[mw] += i64(job.cost)
	}
	return buckets
}

fn top_level_transform_stmt_cost(stmt ast.Stmt) int {
	return transform_stmt_cost(stmt)
}

fn file_transform_cost(file ast.File) int {
	mut cost := 1
	for stmt in file.stmts {
		cost += transform_stmt_cost(stmt)
	}
	return cost
}

fn transform_stmt_cost(stmt ast.Stmt) int {
	mut cost := 1
	match stmt {
		ast.AssertStmt {
			cost += transform_expr_cost(stmt.expr) + transform_expr_cost(stmt.extra)
		}
		ast.AssignStmt {
			for expr in stmt.lhs {
				cost += transform_expr_cost(expr)
			}
			for expr in stmt.rhs {
				cost += transform_expr_cost(expr)
			}
		}
		ast.BlockStmt {
			cost += transform_stmts_cost(stmt.stmts)
		}
		ast.ComptimeStmt {
			cost += transform_stmt_cost(stmt.stmt)
		}
		ast.ConstDecl {
			for field in stmt.fields {
				cost += transform_expr_cost(field.value)
			}
		}
		ast.DeferStmt {
			cost += transform_stmts_cost(stmt.stmts)
		}
		ast.EnumDecl {
			for field in stmt.fields {
				cost += transform_expr_cost(field.value)
			}
		}
		ast.ExprStmt {
			cost += transform_expr_cost(stmt.expr)
		}
		ast.FnDecl {
			cost += 10 + transform_stmts_cost(stmt.stmts)
		}
		ast.ForInStmt {
			cost += transform_expr_cost(stmt.key) + transform_expr_cost(stmt.value) +
				transform_expr_cost(stmt.expr)
		}
		ast.ForStmt {
			cost += transform_stmt_cost(stmt.init) + transform_expr_cost(stmt.cond) +
				transform_stmt_cost(stmt.post) + transform_stmts_cost(stmt.stmts)
		}
		ast.GlobalDecl {
			for field in stmt.fields {
				cost += transform_expr_cost(field.typ) + transform_expr_cost(field.value)
			}
		}
		ast.LabelStmt {
			cost += transform_stmt_cost(stmt.stmt)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				cost += transform_expr_cost(expr)
			}
		}
		ast.StructDecl {
			for field in stmt.fields {
				cost += transform_expr_cost(field.typ) + transform_expr_cost(field.value)
			}
			for expr in stmt.embedded {
				cost += transform_expr_cost(expr)
			}
			for expr in stmt.implements {
				cost += transform_expr_cost(expr)
			}
		}
		else {}
	}

	return cost
}

fn transform_stmts_cost(stmts []ast.Stmt) int {
	mut cost := 0
	for stmt in stmts {
		cost += transform_stmt_cost(stmt)
	}
	return cost
}

fn transform_expr_cost(expr ast.Expr) int {
	mut cost := 1
	match expr {
		ast.ArrayInitExpr {
			cost += transform_expr_cost(expr.init) + transform_expr_cost(expr.cap) +
				transform_expr_cost(expr.len)
			for item in expr.exprs {
				cost += transform_expr_cost(item)
			}
		}
		ast.AsCastExpr, ast.ComptimeExpr, ast.LambdaExpr, ast.ModifierExpr, ast.OrExpr,
		ast.ParenExpr, ast.PostfixExpr, ast.PrefixExpr {
			cost += transform_expr_cost(expr.expr)
		}
		ast.AssocExpr {
			cost += transform_expr_cost(expr.expr)
			for field in expr.fields {
				cost += transform_expr_cost(field.value)
			}
		}
		ast.CallExpr {
			cost += transform_expr_cost(expr.lhs)
			for arg in expr.args {
				cost += transform_expr_cost(arg)
			}
		}
		ast.CallOrCastExpr {
			cost += transform_expr_cost(expr.lhs) + transform_expr_cost(expr.expr)
		}
		ast.CastExpr {
			cost += transform_expr_cost(expr.expr) + transform_expr_cost(expr.typ)
		}
		ast.FieldInit {
			cost += transform_expr_cost(expr.value)
		}
		ast.FnLiteral {
			cost += transform_stmts_cost(expr.stmts)
		}
		ast.GenericArgOrIndexExpr {
			cost += transform_expr_cost(expr.lhs) + transform_expr_cost(expr.expr)
		}
		ast.GenericArgs {
			cost += transform_expr_cost(expr.lhs)
			for arg in expr.args {
				cost += transform_expr_cost(arg)
			}
		}
		ast.IfExpr {
			cost += transform_expr_cost(expr.cond) + transform_stmts_cost(expr.stmts) +
				transform_expr_cost(expr.else_expr)
		}
		ast.IfGuardExpr {
			cost += transform_stmt_cost(expr.stmt)
		}
		ast.IndexExpr {
			cost += transform_expr_cost(expr.lhs) + transform_expr_cost(expr.expr)
		}
		ast.InfixExpr {
			cost += transform_expr_cost(expr.lhs) + transform_expr_cost(expr.rhs)
		}
		ast.InitExpr {
			cost += transform_expr_cost(expr.typ)
			for field in expr.fields {
				cost += transform_expr_cost(field.value)
			}
		}
		ast.KeywordOperator, ast.Tuple {
			for item in expr.exprs {
				cost += transform_expr_cost(item)
			}
		}
		ast.LockExpr {
			for item in expr.lock_exprs {
				cost += transform_expr_cost(item)
			}
			for item in expr.rlock_exprs {
				cost += transform_expr_cost(item)
			}
			cost += transform_stmts_cost(expr.stmts)
		}
		ast.MapInitExpr {
			for item in expr.keys {
				cost += transform_expr_cost(item)
			}
			for item in expr.vals {
				cost += transform_expr_cost(item)
			}
		}
		ast.MatchExpr {
			cost += transform_expr_cost(expr.expr)
			for branch in expr.branches {
				for cond in branch.cond {
					cost += transform_expr_cost(cond)
				}
				cost += transform_stmts_cost(branch.stmts)
			}
		}
		ast.RangeExpr {
			cost += transform_expr_cost(expr.start) + transform_expr_cost(expr.end)
		}
		ast.SelectExpr {
			cost += transform_stmt_cost(expr.stmt) + transform_stmts_cost(expr.stmts) +
				transform_expr_cost(expr.next)
		}
		ast.SelectorExpr {
			cost += transform_expr_cost(expr.lhs)
		}
		ast.SqlExpr {
			cost += transform_expr_cost(expr.expr)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				cost += transform_expr_cost(inter.expr) + transform_expr_cost(inter.format_expr)
			}
		}
		ast.Type {
			cost += transform_type_cost(expr)
		}
		ast.UnsafeExpr {
			cost += transform_stmts_cost(expr.stmts)
		}
		else {}
	}

	return cost
}

fn transform_type_cost(typ ast.Type) int {
	mut cost := 1
	match typ {
		ast.ArrayFixedType {
			cost += transform_expr_cost(typ.elem_type) + transform_expr_cost(typ.len)
		}
		ast.ArrayType, ast.ChannelType, ast.ThreadType {
			cost += transform_expr_cost(typ.elem_type)
		}
		ast.FnType {
			for param in typ.params {
				cost += transform_expr_cost(param.typ)
			}
			cost += transform_expr_cost(typ.return_type)
		}
		ast.GenericType {
			cost += transform_expr_cost(typ.name)
			for param in typ.params {
				cost += transform_expr_cost(param)
			}
		}
		ast.MapType {
			cost += transform_expr_cost(typ.key_type) + transform_expr_cost(typ.value_type)
		}
		ast.OptionType, ast.PointerType, ast.ResultType {
			cost += transform_expr_cost(typ.base_type)
		}
		else {}
	}

	return cost
}

// lpt_buckets distributes file indices across n_jobs workers using the
// longest-processing-time-first heuristic: process files largest-first and
// always append to the currently least-loaded worker. This keeps the heaviest
// files on separate workers so the fan-out wall time approaches
// total_work / n_jobs instead of being pinned to one overloaded contiguous
// chunk. The cost proxy is top-level statement count (cheap, and the giant
// files have proportionally many declarations). Deterministic: files are
// ordered by (cost desc, index asc) and ties pick the lowest worker index.
fn lpt_buckets(files []ast.File, n_jobs int) [][]int {
	n := files.len
	mut cost := []int{len: n}
	for i in 0 .. n {
		// Cost proxy: count function bodies, not just top-level declarations, so
		// a file of a few huge functions (transformer.v, the cleanc gen files)
		// outranks one with many tiny ones. Deterministic; one level deep is
		// enough to separate the heavyweight files that drove the imbalance.
		mut c := 1
		for stmt in files[i].stmts {
			c++
			if stmt is ast.FnDecl {
				c += stmt.stmts.len
			}
		}
		cost[i] = c
	}
	// order = file indices by cost descending. Implemented as a plain insertion
	// sort (n is small, a few hundred) rather than sort_with_compare: this file
	// must self-host through every backend, and capturing closures / pointer
	// comparators are not reliably codegen'd by the v2 cleanc and arm64 paths.
	// Stable on index (only shifts on strictly-greater), so deterministic.
	mut order := []int{len: n, init: index}
	for i in 1 .. n {
		key := order[i]
		kc := cost[key]
		mut j := i - 1
		for j >= 0 && cost[order[j]] < kc {
			order[j + 1] = order[j]
			j--
		}
		order[j + 1] = key
	}
	mut buckets := [][]int{len: n_jobs}
	mut load := []i64{len: n_jobs}
	for fi in order {
		mut mw := 0
		for w in 1 .. n_jobs {
			if load[w] < load[mw] {
				mw = w
			}
		}
		buckets[mw] << fi
		load[mw] += i64(cost[fi])
	}
	return buckets
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
