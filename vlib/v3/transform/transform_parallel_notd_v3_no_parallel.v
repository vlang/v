module transform

import os
import runtime
import sync
import time
import v3.flat
import v3.types
import v3.workers

// Parallel function-body transform. Each worker transforms a disjoint set of
// closure-free functions on its own cloned AST + forked TypeChecker, and the
// master folds the results back together in a fixed order so the build stays
// deterministic.

const min_parallel_transform_items = 256
const max_parallel_transform_jobs = 7
// Shared-base workers share the AST but retain private checker and transform
// scratch. Eight lanes keep large user builds below the ordinary memory ceiling.
const max_shared_transform_jobs = 8
// One chunk per lane bounds the number of private worker views kept until merge.
const shared_transform_chunks_per_job = 1
// Normal function lowering needs part of the shared append pool too. Limit
// expansion estimates to the other half before assigning fixed worker regions.
const shared_expansion_pool_divisor = 2
const max_parallel_monomorph_jobs = 18
// Recycle scratch arenas throughout large self-hosting transforms.
const scoped_transform_batches = 16
const scoped_transform_max_batch_items = 2048
const scoped_monomorph_batch_specs = 512
const scoped_monomorph_node_threshold = 1_000_000

$if !windows {
	// RegionRelocateArgs is one worker region's in-place id-relocation job: the
	// shifts are precomputed from the (deterministic) region content lengths, so
	// disjoint regions relocate concurrently before the serial compaction pass.
	struct RegionRelocateArgs {
		worker      voidptr // &Transformer (region view)
		node_start  int
		node_end    int
		child_start int
		child_end   int
		node_shift  i32
		child_shift i32
	}

	fn region_relocate_thread(arg voidptr) voidptr {
		a := unsafe { &RegionRelocateArgs(arg) }
		mut w := unsafe { &Transformer(a.worker) }
		w.relocate_region_in_place(a.node_start, a.node_end, a.child_start, a.child_end,
			a.node_shift, a.child_shift)
		return unsafe { nil }
	}

	// transform_pre_scan_index_thread builds the AST/tc-only prepare() indexes
	// (deferred via defer_pre_scan_indexes) on a helper thread, directly on the
	// master transformer: the fields it writes are untouched by the concurrent
	// collect_types walk, and the permanent thread arena keeps them alive. Alias
	// method collection stays on the master because its normalization reads the
	// type maps collect_types is still populating.
	fn transform_pre_scan_index_thread(arg voidptr) voidptr {
		mut t := unsafe { &Transformer(arg) }
		t.build_source_parent_index()
		t.collect_multi_return_fn_ret_types()
		t.rebuild_variadic_suffix_index()
		return unsafe { nil }
	}

	// transform_const_fixed_scan_thread classifies array-literal constants on a
	// helper thread while the master builds its lookup indexes in prepare().
	// The scan worker only reads the immutable AST and post-check tc tables,
	// plus its own private const-suffix map, so it never observes the maps the
	// master is concurrently building.
	fn transform_const_fixed_scan_thread(arg voidptr) voidptr {
		mut w := unsafe { &Transformer(arg) }
		scope := transform_worker_scope_begin(w.scope_parallel_workers)
		w.collect_const_suffixes()
		w.precompute_const_array_fixed_storage()
		w.worker_scope = scope
		transform_worker_scope_leave(scope)
		return unsafe { nil }
	}

	// transform_param_prep_thread snapshots function parameter types while the
	// master prepares its independent type indexes. The worker owns its checker
	// caches and publishes only its completed declaration maps after joining.
	fn transform_param_prep_thread(arg voidptr) voidptr {
		mut w := unsafe { &Transformer(arg) }
		w.prepare_parallel_call_param_types()
		return unsafe { nil }
	}

	// TransformChunkArgs is the payload handed to each persistent worker.
	struct TransformChunkArgs {
		worker    voidptr // &Transformer
		items_ptr voidptr // &[]FnWorkItem
	}

	// transform_chunk_thread runs one worker's chunk of function bodies.
	fn transform_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &TransformChunkArgs(arg) }
		mut w := unsafe { &Transformer(a.worker) }
		items := unsafe { &[]FnWorkItem(a.items_ptr) }
		if w.scope_parallel_workers && w.retain_worker_results {
			w.transform_scoped_helper_batches(*items, 1)
		} else {
			w.transform_pure_items_serial(*items)
		}
		return unsafe { nil }
	}

	// shared_chunk_thread runs one shared-base worker's chunk. No clone, no
	// chain: every worker was fully built by the master before spawning.
	fn shared_chunk_thread(arg voidptr) voidptr {
		mut a := unsafe { &SharedChunkArgs(arg) }
		mut w := unsafe { &Transformer(a.worker) }
		items := unsafe { &[]FnWorkItem(a.items_ptr) }
		mut csw := time.new_stopwatch()
		if w.scope_parallel_workers && (!a.is_master || w.retain_worker_results) {
			w.transform_scoped_helper_batches(*items, scoped_transform_batches)
		} else {
			w.transform_pure_items_serial(*items)
		}
		$if v3_ttime ? {
			mut cost := i64(0)
			for it in *items {
				cost += i64(it.cost) + 1
			}
			a.cost = cost
			a.elapsed_us = csw.elapsed().microseconds()
		}
		return unsafe { nil }
	}
}

// SharedChunkArgs is the payload handed to each shared-base worker thread.
struct SharedChunkArgs {
	worker    voidptr // &Transformer
	items_ptr voidptr // &[]FnWorkItem
	is_master bool
mut:
	cost       i64
	elapsed_us i64
}

// ScopedTextScanArgs is the payload for one scoped-text flag-scan worker.
struct ScopedTextScanArgs {
	a     &flat.FlatAst = unsafe { nil }
	scope voidptr
	start int
	end   int
	flags voidptr // &u8 base of the per-node flag array
}

struct WorkerScopeFreeArgs {
	scopes []voidptr
	start  int
	end    int
}

// node_has_scoped_text reports whether any text payload of `node` lives in
// `scope`'s arena, i.e. whether canonicalization/promotion would rewrite it.
@[inline]
fn node_has_scoped_text(node &flat.Node, scope voidptr) bool {
	if node.value.len > 0 && transform_scope_owns(scope, node.value.str) {
		return true
	}
	if node.typ.len > 0 && transform_scope_owns(scope, node.typ.str) {
		return true
	}
	if isnil(node.payload) {
		return false
	}
	if transform_scope_owns(scope, node.payload) {
		return true
	}
	params := node.payload.generic_params
	if params.len == 0 {
		return false
	}
	if transform_scope_owns(scope, params.data) {
		return true
	}
	for param in params {
		if param.len > 0 && transform_scope_owns(scope, param.str) {
			return true
		}
	}
	return false
}

$if !windows {
	// scoped_text_scan_thread flags this worker's node range. Pure reads plus
	// byte writes into a disjoint flag range: no allocations, so it is safe to
	// run on pool threads regardless of their arena state.
	fn scoped_text_scan_thread(arg voidptr) voidptr {
		a := unsafe { &ScopedTextScanArgs(arg) }
		flags := unsafe { &u8(a.flags) }
		for idx in a.start .. a.end {
			node := unsafe { &a.a.nodes[idx] }
			if node_has_scoped_text(node, a.scope) {
				unsafe {
					flags[idx] = 1
				}
			}
		}
		return unsafe { nil }
	}
}

// TopLevelKindScanArgs is the payload for one top-level-kind flag-scan worker.
struct TopLevelKindScanArgs {
	a                 &flat.FlatAst      = unsafe { nil }
	tc                &types.TypeChecker = unsafe { nil }
	start             int
	end               int
	flags             voidptr // &u8 base of the per-node flag array (index 0 == node `base`)
	escape_flags      voidptr // optional &u8 base for escape-precheck candidates
	base              int
	prefix_param_scan bool
}

$if !windows {
	// literal_decl_scan_thread marks the sparse node kinds needed to associate
	// a function literal with its containing top-level declaration. The low
	// nibble is also a cheap transform-cost weight used to balance fn workers.
	// Genuine top-level boundaries are marked by the master after the scan: node
	// kind alone cannot distinguish a top-level type from a local type declaration.
	fn literal_decl_scan_thread(arg voidptr) voidptr {
		a := unsafe { &TopLevelKindScanArgs(arg) }
		flags := unsafe { &u8(a.flags) }
		escape_flags := unsafe { &u8(a.escape_flags) }
		for i in a.start .. a.end {
			node := unsafe { &a.a.nodes[i] }
			mut flag := match node.kind {
				.call, .struct_init {
					u8(8)
				}
				.selector {
					u8(6)
				}
				.assign, .decl_assign, .selector_assign, .index_assign {
					u8(5)
				}
				.array_literal, .array_init, .map_init, .fn_literal, .lambda_expr, .string_interp {
					u8(4)
				}
				.index, .if_expr, .match_stmt, .for_stmt, .for_in_stmt, .select_stmt {
					u8(3)
				}
				.infix, .cast_expr, .as_expr, .or_expr, .return_stmt {
					u8(2)
				}
				else {
					u8(1)
				}
			}
			if node.kind in [.fn_literal, .lambda_expr] {
				flag |= 16
			}
			if node.kind == .fn_decl {
				flag |= 32
			} else if node.kind in [.const_decl, .global_decl] {
				flag |= 64
			}
			unsafe {
				flags[i - a.base] = flag
			}
			mut may_escape := node.kind == .prefix && node.op == .amp
			if !may_escape && node.kind == .call && node.children_count > 1 {
				name := a.tc.resolved_call_name(flat.NodeId(i)) or {
					unsafe {
						escape_flags[i - a.base] = 1
					}
					continue
				}
				params := a.tc.fn_param_types[name] or {
					unsafe {
						escape_flags[i - a.base] = 1
					}
					continue
				}
				for param in params {
					if escape_type_is_void_pointer(param) {
						may_escape = true
						break
					}
				}
			}
			if may_escape {
				unsafe {
					escape_flags[i - a.base] = 1
				}
			}
		}
		return unsafe { nil }
	}

	// top_level_kind_scan_thread flags file/module nodes and generic-candidate
	// fn decls in this worker's range. Pure reads plus byte writes into a
	// disjoint flag range: no allocations, so it is safe on pool threads in any
	// arena state. Candidate detection uses only the state-free placeholder
	// prescreen, a strict superset of generic_fn_decl_needs_erasure_scan.
	fn top_level_kind_scan_thread(arg voidptr) voidptr {
		a := unsafe { &TopLevelKindScanArgs(arg) }
		flags := unsafe { &u8(a.flags) }
		for i in a.start .. a.end {
			node := unsafe { &a.a.nodes[i] }
			if node.kind == .file || node.kind == .module_decl {
				unsafe {
					flags[i - a.base] = 1
				}
			} else if node.kind == .fn_decl
				&& fn_decl_generic_candidate_prescreen(a.a, node, a.prefix_param_scan) {
				unsafe {
					flags[i - a.base] = 1
				}
			}
		}
		return unsafe { nil }
	}
}

// scan_literal_decl_flags_parallel fills the sparse literal/declaration flags
// used by collect_literal_fn_decls. The master can then word-scan the compact
// byte array instead of streaming every full AST node header.
fn scan_literal_decl_flags_parallel(t &Transformer, limit int, mut flags []u8, mut escape_flags []u8) bool {
	$if windows {
		return false
	} $else {
		a := t.a
		if isnil(t.tc) || isnil(a.worker_pool) || limit < 65536 || limit > a.nodes.len
			|| flags.len < limit || escape_flags.len < limit || t.tc.top_level_idx.len == 0
			|| t.tc.top_level_idx_nodes_len != limit {
			return false
		}
		n_jobs := a.worker_pool.size() + 1
		chunk := (limit + n_jobs - 1) / n_jobs
		mut args := []TopLevelKindScanArgs{cap: n_jobs}
		for ji in 0 .. n_jobs {
			start := ji * chunk
			mut end := start + chunk
			if end > limit {
				end = limit
			}
			if start >= end {
				break
			}
			args << TopLevelKindScanArgs{
				a:            a
				tc:           t.tc
				start:        start
				end:          end
				flags:        flags.data
				escape_flags: escape_flags.data
			}
		}
		mut tasks := []workers.Task{cap: args.len}
		for ji in 0 .. args.len {
			tasks << workers.Task{
				run:        literal_decl_scan_thread
				arg:        unsafe { voidptr(&args[ji]) }
				force_sync: ji == 0
			}
		}
		a.worker_pool.run(tasks)
		// top_level_idx also contains synthesized anonymous/function-local type
		// declarations so later passes can find them. They are not subtree
		// boundaries: an escape candidate before one still belongs to the enclosing
		// function. Exclude those exact ids while marking reset points.
		mut synthetic_pos := 0
		for idx in t.tc.top_level_idx {
			for synthetic_pos < t.tc.synthetic_top_level_type_ids.len
				&& t.tc.synthetic_top_level_type_ids[synthetic_pos] < idx {
				synthetic_pos++
			}
			if synthetic_pos < t.tc.synthetic_top_level_type_ids.len
				&& t.tc.synthetic_top_level_type_ids[synthetic_pos] == idx {
				synthetic_pos++
				continue
			}
			if idx >= 0 && idx < limit {
				flags[idx] |= u8(128)
			}
		}
		return true
	}
}

// fn_decl_generic_candidate_prescreen is the allocation-free superset filter
// for generic_fn_decl_needs_erasure_scan: explicit generic params, or a
// possible placeholder in the signature texts. Fn decls failing it can never
// need erasure.
@[inline]
fn fn_decl_generic_candidate_prescreen(a &flat.FlatAst, node &flat.Node, prefix_param_scan bool) bool {
	if !isnil(node.payload) && node.payload.generic_params.len > 0 {
		return true
	}
	if generic_placeholder_prescreen(node.typ) || generic_placeholder_prescreen(node.value) {
		return true
	}
	for i in 0 .. node.children_count {
		child := a.child_node(node, i)
		if child.kind != .param {
			if prefix_param_scan {
				break
			}
			continue
		}
		if generic_placeholder_prescreen(child.typ) {
			return true
		}
	}
	return false
}

// scan_top_level_kind_flags_parallel fills `flags` (one byte per node id in
// [base, base + flags.len)) for file/module/fn-decl nodes, using the shared
// worker pool. Returns false when no pool is available so the caller can walk
// the range serially instead.
fn scan_top_level_kind_flags_parallel(a &flat.FlatAst, base int, mut flags []u8, prefix_param_scan bool) bool {
	$if windows {
		return false
	} $else {
		n := flags.len
		if isnil(a.worker_pool) || n < 65536 || base < 0 || base + n > a.nodes.len {
			return false
		}
		n_jobs := a.worker_pool.size() + 1
		chunk := (n + n_jobs - 1) / n_jobs
		mut args := []TopLevelKindScanArgs{cap: n_jobs}
		for ji in 0 .. n_jobs {
			start := base + ji * chunk
			mut end := start + chunk
			if end > base + n {
				end = base + n
			}
			if start >= end {
				break
			}
			args << TopLevelKindScanArgs{
				a:                 a
				start:             start
				end:               end
				flags:             flags.data
				base:              base
				prefix_param_scan: prefix_param_scan
			}
		}
		mut tasks := []workers.Task{cap: args.len}
		for ji in 0 .. args.len {
			tasks << workers.Task{
				run:        top_level_kind_scan_thread
				arg:        unsafe { voidptr(&args[ji]) }
				force_sync: ji == 0
			}
		}
		a.worker_pool.run(tasks)
		return true
	}
}

// ScopedTextPromoteArgs is the payload for one fused text-promotion worker.
struct ScopedTextPromoteArgs {
	a     &flat.FlatAst = unsafe { nil }
	scope voidptr
	start int
	end   int
}

$if !windows {
	// scoped_text_promote_thread publishes scope-owned node texts for this
	// worker's id range: text-table hits reuse the canonical entry (read-only
	// map probe), misses clone into the worker's persistent arena. Node slot
	// writes are range-disjoint, so no synchronization is needed.
	fn scoped_text_promote_thread(arg voidptr) voidptr {
		a := unsafe { &ScopedTextPromoteArgs(arg) }
		mut ast := unsafe { &flat.FlatAst(voidptr(a.a)) }
		for idx in a.start .. a.end {
			mut node := unsafe { &ast.nodes[idx] }
			if node.value.len > 0 && transform_scope_owns(a.scope, node.value.str) {
				node.value = promote_scoped_text_read_only(ast, node.value)
			}
			if node.typ.len > 0 && transform_scope_owns(a.scope, node.typ.str) {
				node.typ = promote_scoped_text_read_only(ast, node.typ)
			}
			if isnil(node.payload) {
				continue
			}
			params := node.payload.generic_params
			if params.len == 0 {
				continue
			}
			mut needs := transform_scope_owns(a.scope, node.payload)
				|| transform_scope_owns(a.scope, params.data)
			if !needs {
				for param in params {
					if param.len > 0 && transform_scope_owns(a.scope, param.str) {
						needs = true
						break
					}
				}
			}
			if !needs {
				continue
			}
			mut promoted := []string{cap: params.len}
			for param in params {
				if param.len > 0 && transform_scope_owns(a.scope, param.str) {
					promoted << promote_scoped_text_read_only(ast, param)
				} else {
					promoted << param
				}
			}
			node.set_generic_params(promoted)
		}
		return unsafe { nil }
	}

	fn promote_scoped_text_read_only(a &flat.FlatAst, value string) string {
		if id := a.text_ids[value] {
			return a.text_values[int(id) - 1]
		}
		return value.clone()
	}
}

// promote_scoped_texts_parallel publishes every transform-scope-owned node
// text over the shared worker pool, replacing the serial canonicalize +
// promote pair for builds without retained regions. Returns false when no pool
// is available so the caller can run the serial walks instead.
pub fn promote_scoped_texts_parallel(mut a flat.FlatAst, scope voidptr) bool {
	$if windows {
		return false
	} $else {
		n := a.nodes.len
		if isnil(a.worker_pool) || n < 65536 {
			return false
		}
		n_jobs := a.worker_pool.size() + 1
		chunk := (n + n_jobs - 1) / n_jobs
		mut args := []ScopedTextPromoteArgs{cap: n_jobs}
		for ji in 0 .. n_jobs {
			start := ji * chunk
			mut end := start + chunk
			if end > n {
				end = n
			}
			if start >= end {
				break
			}
			args << ScopedTextPromoteArgs{
				a:     a
				scope: scope
				start: start
				end:   end
			}
		}
		mut tasks := []workers.Task{cap: args.len}
		for ji in 0 .. args.len {
			tasks << workers.Task{
				run:        scoped_text_promote_thread
				arg:        unsafe { voidptr(&args[ji]) }
				force_sync: ji == 0
			}
		}
		a.worker_pool.run(tasks)
		return true
	}
}

// CheckerCachePromoteArgs is the payload for one checker-cache promote worker.
struct CheckerCachePromoteArgs {
	tc              voidptr // &types.TypeChecker
	scope           voidptr
	start           int
	end             int
	generated_start int
}

$if !windows {
	fn worker_scope_free_thread(arg voidptr) voidptr {
		a := unsafe { &WorkerScopeFreeArgs(arg) }
		for i in a.start .. a.end {
			transform_worker_scope_free(a.scopes[i])
		}
		return unsafe { nil }
	}

	// checker_cache_promote_thread publishes scope-owned resolved-call /
	// fn-value strings and clones generated-range expression types for this
	// worker's id range. Slot writes are range-disjoint; clones allocate in the
	// worker's persistent arena, which is never released.
	fn checker_cache_promote_thread(arg voidptr) voidptr {
		a := unsafe { &CheckerCachePromoteArgs(arg) }
		mut tc := unsafe { &types.TypeChecker(a.tc) }
		for idx in a.start .. a.end {
			if idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
				name := tc.resolved_call_names[idx]
				if name.len > 0 && transform_scope_owns(a.scope, name.str) {
					tc.resolved_call_names[idx] = name.clone()
				}
			}
			if idx < tc.resolved_fn_value_set.len && tc.resolved_fn_value_set[idx] {
				name := tc.resolved_fn_value_names[idx]
				if name.len > 0 && transform_scope_owns(a.scope, name.str) {
					tc.resolved_fn_value_names[idx] = name.clone()
				}
			}
			if idx >= a.generated_start && idx < tc.expr_type_set.len && tc.expr_type_set[idx]
				&& idx < tc.expr_type_values.len {
				tc.expr_type_values[idx] = types.clone_owned_type(tc.expr_type_values[idx])
			}
		}
		return unsafe { nil }
	}
}

// free_worker_scopes_parallel releases independent retained worker arenas on
// the shared pool. Returns false when a serial release is cheaper or required.
fn free_worker_scopes_parallel(a &flat.FlatAst, scopes []voidptr) bool {
	$if windows {
		return false
	} $else {
		if isnil(a.worker_pool) || scopes.len < 4 {
			return false
		}
		n_jobs := if scopes.len < a.worker_pool.size() + 1 {
			scopes.len
		} else {
			a.worker_pool.size() + 1
		}
		chunk := (scopes.len + n_jobs - 1) / n_jobs
		mut args := []WorkerScopeFreeArgs{cap: n_jobs}
		for ji in 0 .. n_jobs {
			start := ji * chunk
			mut end := start + chunk
			if end > scopes.len {
				end = scopes.len
			}
			if start >= end {
				break
			}
			args << WorkerScopeFreeArgs{
				scopes: scopes
				start:  start
				end:    end
			}
		}
		mut tasks := []workers.Task{cap: args.len}
		for ji in 0 .. args.len {
			tasks << workers.Task{
				run:        worker_scope_free_thread
				arg:        unsafe { voidptr(&args[ji]) }
				force_sync: ji == 0
			}
		}
		a.worker_pool.run(tasks)
		return true
	}
}

// promote_scoped_checker_node_caches_parallel runs the per-id loops of the
// checker node-cache promotion over the shared worker pool. Returns false when
// no pool is available so the caller can run them serially instead.
pub fn promote_scoped_checker_node_caches_parallel(mut tc types.TypeChecker, a &flat.FlatAst, scope voidptr, generated_start int) bool {
	$if windows {
		return false
	} $else {
		n := tc.resolved_call_names.len
		if isnil(a.worker_pool) || n < 65536 || os.getenv('V3_NO_PAR_CHECKER_PROMOTE') != '' {
			return false
		}
		n_jobs := a.worker_pool.size() + 1
		chunk := (n + n_jobs - 1) / n_jobs
		mut args := []CheckerCachePromoteArgs{cap: n_jobs}
		for ji in 0 .. n_jobs {
			start := ji * chunk
			mut end := start + chunk
			if end > n {
				end = n
			}
			if start >= end {
				break
			}
			args << CheckerCachePromoteArgs{
				tc:              voidptr(tc)
				scope:           scope
				start:           start
				end:             end
				generated_start: generated_start
			}
		}
		mut tasks := []workers.Task{cap: args.len}
		for ji in 0 .. args.len {
			tasks << workers.Task{
				run:        checker_cache_promote_thread
				arg:        unsafe { voidptr(&args[ji]) }
				force_sync: ji == 0
			}
		}
		a.worker_pool.run(tasks)
		return true
	}
}

// scan_scoped_text_flags_parallel fills `flags` (one byte per node id in
// [0, flags.len)) for nodes whose text is owned by `scope`, using the shared
// worker pool. Returns false when no pool is available so the caller can walk
// every node serially instead.
pub fn scan_scoped_text_flags_parallel(a &flat.FlatAst, scope voidptr, mut flags []u8) bool {
	$if windows {
		return false
	} $else {
		n := flags.len
		if isnil(a.worker_pool) || n < 65536 {
			return false
		}
		n_jobs := a.worker_pool.size() + 1
		chunk := (n + n_jobs - 1) / n_jobs
		mut args := []ScopedTextScanArgs{cap: n_jobs}
		for ji in 0 .. n_jobs {
			start := ji * chunk
			mut end := start + chunk
			if end > n {
				end = n
			}
			if start >= end {
				break
			}
			args << ScopedTextScanArgs{
				a:     a
				scope: scope
				start: start
				end:   end
				flags: flags.data
			}
		}
		mut tasks := []workers.Task{cap: args.len}
		for ji in 0 .. args.len {
			tasks << workers.Task{
				run:        scoped_text_scan_thread
				arg:        unsafe { voidptr(&args[ji]) }
				force_sync: ji == 0
			}
		}
		a.worker_pool.run(tasks)
		return true
	}
}

struct MonomorphChunkArgs {
	worker        voidptr // &Transformer
	claims        &MonomorphClaimState = unsafe { nil }
	is_master     bool
	worker_idx    int
	base_nodes    int
	base_children int
	node_start    int
	child_start   int
	struct_decls  map[string]GenericStructDecl
	sum_decls     map[string]GenericSumDecl
mut:
	roots         []flat.NodeId
	emitted_specs []PendingGenericFnSpec
	generated     []string
	scan_nodes    []int
	struct_specs  map[string]string
	sum_specs     map[string]GenericSpecContext
	scope         voidptr
}

struct MonomorphScanArgs {
	a     &flat.FlatAst = unsafe { nil }
	start int
	end   int
mut:
	nodes []int
}

@[heap]
struct MonomorphClaimState {
mut:
	mu          &sync.Mutex = unsafe { nil }
	cond        &sync.Cond  = unsafe { nil }
	claimed     map[string]bool
	queues      [][]PendingGenericFnSpec
	queue_costs []i64
	remaining   int
}

$if !windows {
	fn monomorph_scan_thread(arg voidptr) voidptr {
		mut scan := unsafe { &MonomorphScanArgs(arg) }
		mut nodes := []int{cap: (scan.end - scan.start) / 16}
		for i in scan.start .. scan.end {
			if scan.a.nodes[i].kind in [.call, .index, .index_assign] {
				nodes << i
			}
		}
		scan.nodes = unsafe { nodes }
		return unsafe { nil }
	}

	fn monomorph_chunk_thread(arg voidptr) voidptr {
		mut a := unsafe { &MonomorphChunkArgs(arg) }
		mut w := unsafe { &Transformer(a.worker) }
		mut scope := unsafe { nil }
		if !a.is_master {
			scope = transform_worker_scope_begin(w.scope_parallel_workers)
		}
		w.parallel_monomorph_worker = true
		// A worker can discover a nested specialization that another worker will
		// emit. Register that signature in the discovering worker immediately so
		// it can transform the current call with the correct return type.
		w.generic_signatures_pre_registered = false
		w.defer_nested_generic_emissions = true
		w.generic_clone_children.ensure_cap(65536)
		generated_start := w.a.nodes.len
		mut roots := []flat.NodeId{cap: 64}
		mut emitted_specs := []PendingGenericFnSpec{cap: 64}
		mut generated := []string{}
		mut struct_specs := map[string]string{}
		mut sum_specs := map[string]GenericSpecContext{}
		mut private_region := false
		mut claims := a.claims
		for {
			claims.mu.lock()
			mut queue_idx := 0
			for claims.remaining > 0 {
				mut heaviest := i64(0)
				for idx, cost in claims.queue_costs {
					if cost > heaviest {
						heaviest = cost
						queue_idx = idx
					}
				}
				if heaviest > 0 && claims.queues[queue_idx].len > 0 {
					break
				}
				claims.cond.wait()
			}
			if claims.remaining == 0 {
				claims.mu.unlock()
				break
			}
			if claims.queues[queue_idx].len == 0 {
				// A condition variable may wake spuriously. Recheck all queues.
				claims.mu.unlock()
				continue
			}
			spec := claims.queues[queue_idx].pop()
			claims.queue_costs[queue_idx] -= i64(w.generic_decl_source_cost(spec.decl))
			claims.mu.unlock()
			if !w.generic_specialization_registered(spec.decl, spec.args) {
				value := specialized_generic_fn_value(spec.decl.node.value, spec.args)
				w.register_specialized_fn_signature_value(spec.decl, value, spec.args)
			}
			if !private_region && !w.monomorph_worker_has_headroom(spec) {
				w.detach_monomorph_worker_region(a.base_nodes, a.base_children, a.node_start,
					a.child_start, spec)
				private_region = true
			}
			spec_nodes_start := w.a.nodes.len
			root := w.emit_generic_fn_specialization(spec.decl, spec.args)
			generated << w.generated_fn_used_names(spec.decl, root, spec.args)
			for i in spec_nodes_start .. w.a.nodes.len {
				node := w.a.nodes[i]
				w.collect_generic_struct_specs_from_node(node, spec.decl.module, spec.decl.file,
					true, a.struct_decls, mut struct_specs)
				w.collect_generic_sum_specs_from_node(node, spec.decl.module, spec.decl.file,
					a.sum_decls, mut sum_specs)
			}
			roots << root
			emitted_specs << spec
			pending := w.pending_generic_fn_specs
			w.pending_generic_fn_specs = []PendingGenericFnSpec{}
			claims.mu.lock()
			for request in pending {
				if !claims.claimed[request.key] {
					claims.claimed[request.key] = true
					target := monomorph_spec_worker(request.key, claims.queues.len)
					claims.queues[target] << request
					claims.queue_costs[target] += i64(w.generic_decl_source_cost(request.decl))
					claims.remaining++
				} else {
					w.pending_generic_fn_spec_keys.delete(request.key)
				}
			}
			claims.remaining--
			claims.cond.broadcast()
			claims.mu.unlock()
		}
		mut scan_nodes := []int{cap: (w.a.nodes.len - generated_start) / 8}
		for i in generated_start .. w.a.nodes.len {
			if w.a.nodes[i].kind in [.call, .index, .index_assign] {
				scan_nodes << i
			}
		}
		if !a.is_master {
			w.worker_scope = scope
			transform_worker_scope_leave(scope)
		}
		a.roots = roots
		a.emitted_specs = emitted_specs
		a.generated = generated
		a.scan_nodes = scan_nodes.clone()
		a.struct_specs = struct_specs.move()
		a.sum_specs = sum_specs.move()
		a.scope = scope
		return unsafe { nil }
	}

	struct InterfaceBoxScanArgs {
		source voidptr // &Transformer
		start  int
		end    int
		file   string
		module string
	mut:
		worker voidptr // &Transformer
		scope  voidptr
	}

	fn interface_box_scan_thread(arg voidptr) voidptr {
		mut a := unsafe { &InterfaceBoxScanArgs(arg) }
		source := unsafe { &Transformer(a.source) }
		scope := transform_worker_scope_begin(source.scope_parallel_workers)
		wtc := source.tc.fork_for_parallel_transform(source.a)
		mut scan := source.fork_scan_worker(wtc)
		scan.cur_file = a.file
		scan.cur_module = a.module
		scan.tc.cur_file = a.file
		scan.tc.cur_module = a.module
		scan.interface_boxed_types_done = true
		scan.collect_interface_boxed_types_range(a.start, a.end)
		transform_worker_scope_leave(scope)
		a.worker = voidptr(scan)
		a.scope = scope
		return unsafe { nil }
	}
}

fn (mut t Transformer) prepare_parallel_monomorph_scan(start int, end int) bool {
	$if windows {
		return false
	} $else {
		if isnil(t.a.worker_pool) || end - start < 65536 {
			return false
		}
		n_jobs := t.a.worker_pool.size() + 1
		chunk := (end - start + n_jobs - 1) / n_jobs
		mut args := []MonomorphScanArgs{cap: n_jobs}
		for ji in 0 .. n_jobs {
			chunk_start := start + ji * chunk
			chunk_end := int_min(chunk_start + chunk, end)
			if chunk_start >= chunk_end {
				break
			}
			args << MonomorphScanArgs{
				a:     t.a
				start: chunk_start
				end:   chunk_end
			}
		}
		mut tasks := []workers.Task{cap: args.len}
		for ji in 0 .. args.len {
			tasks << workers.Task{
				run:        monomorph_scan_thread
				arg:        unsafe { voidptr(&args[ji]) }
				force_sync: ji == 0
			}
		}
		t.a.worker_pool.run(tasks)
		mut count := 0
		for arg in args {
			count += arg.nodes.len
		}
		mut nodes := []int{cap: count}
		for arg in args {
			nodes << arg.nodes
		}
		t.parallel_monomorph_scan_nodes = unsafe { nodes }
		t.parallel_monomorph_scan_start = start
		t.parallel_monomorph_scan_end = end
		return true
	}
}

fn (mut t Transformer) run_parallel_monomorphize_specs(specs []PendingGenericFnSpec, struct_decls map[string]GenericStructDecl, sum_decls map[string]GenericSumDecl, mut emitted map[string]bool, mut generated []string) bool {
	$if windows {
		return false
	} $else {
		$if linux && arm64 {
			// Shared append-only AST regions intermittently corrupt the heap on
			// Linux/ARM64. Keep the safe serial monomorphizer on that target while
			// retaining the other parallel compiler stages.
			return false
		}
		if specs.len == 0 {
			return false
		}
		if t.scope_parallel_workers && t.scoped_monomorphize
			&& t.a.nodes.len >= scoped_monomorph_node_threshold {
			return t.run_scoped_monomorphize_specs(specs, mut emitted, mut generated)
		}
		if isnil(t.a.worker_pool) {
			worker_count := runtime.nr_jobs() - 1
			if worker_count <= 0 {
				return false
			}
			t.a.worker_pool = workers.new(worker_count)
		}
		available_jobs := t.a.worker_pool.size() + 1
		configured_jobs := os.getenv('V3_MONOMORPH_JOBS').int()
		job_limit := monomorph_job_limit(available_jobs, t.a.nodes.len, configured_jobs)
		n_jobs := monomorph_job_count(job_limit, specs.len)
		if n_jobs <= 1 {
			return false
		}
		debug_started := time.ticks()
		base_nodes := t.a.nodes.len
		base_children := t.a.children.len
		// Initial call sites can expose a much larger nested generic closure.
		// Give every append-only worker region enough headroom for that closure;
		// the arrays still retain only the nodes actually merged by the master.
		// Keep a full nested-specialization cushion per worker. A single shared
		// cushion gets divided into tiny regions and makes normal compiler-sized
		// batches detach, copying the entire immutable base AST in every worker.
		// The private growing fallback below remains available for truly uneven
		// batches that exceed their region.
		// Volt's measured closure averages about 1k nodes per specialization,
		// with the largest hash partition below 130k nodes. Keep enough shared
		// space for the normal closure without forcing the backing slabs to grow
		// to several times their retained size. An unusually uneven partition
		// still uses the private-region fallback below.
		node_reserve := specs.len * 256 + n_jobs * 196608
		child_reserve := specs.len * 320 + n_jobs * 229376
		t.a.nodes.ensure_cap(base_nodes + node_reserve)
		t.a.children.ensure_cap(base_children + child_reserve)
		t.monomorph_profile('mono capacity: ${time.ticks() - debug_started} ms')
		mut node_pool := t.a.nodes.cap - base_nodes
		mut child_pool := t.a.children.cap - base_children
		// The regression test deliberately makes every region tiny so private
		// growth is covered without needing a multi-million-node input.
		if os.getenv('V3_TEST_MONOMORPH_GROW') == '1' {
			node_pool = n_jobs * 64
			child_pool = n_jobs * 64
		}
		mut node_starts := []int{len: n_jobs + 1}
		mut child_starts := []int{len: n_jobs + 1}
		for i in 0 .. n_jobs + 1 {
			node_starts[i] = base_nodes + node_pool * i / n_jobs
			child_starts[i] = base_children + child_pool * i / n_jobs
		}
		// Each monomorph worker can lift function literals while specializing its
		// generic bodies. Give every worker a disjoint deterministic name range;
		// private checker snapshots cannot see names created concurrently by peers.
		t.global_temp_counter = node_starts[0]

		// Monomorphization uses a fresh Transformer, so its declaration cache has
		// not been warmed by the earlier function-body transform. Build it before
		// the forks; lazy initialization from several workers corrupts the map.
		t.prepare_parallel_call_param_types()
		// Reflected generic JSON bodies query loop-variable roles while every
		// specialization is cloned. Build the immutable source-template index once
		// on the master instead of making every worker scan the multi-million-node
		// AST independently on its first reflected field.
		t.prepare_comptime_reflected_for_roles()
		t.tc.freeze_type_cache_for_forks()
		setup_scope := transform_worker_scope_begin(t.scope_parallel_workers)
		decls := t.cached_generic_fn_decls()
		mut claims := &MonomorphClaimState{
			mu:          sync.new_mutex()
			claimed:     map[string]bool{}
			queues:      [][]PendingGenericFnSpec{len: n_jobs}
			queue_costs: []i64{len: n_jobs}
			remaining:   specs.len
		}
		claims.cond = sync.new_cond(claims.mu)
		for spec in specs {
			claims.claimed[spec.key] = true
			target := monomorph_spec_worker(spec.key, n_jobs)
			claims.queues[target] << spec
			claims.queue_costs[target] += i64(t.generic_decl_source_cost(spec.decl))
		}
		mut args := []MonomorphChunkArgs{len: n_jobs}
		args[0] = MonomorphChunkArgs{
			worker:        voidptr(t)
			claims:        claims
			is_master:     true
			worker_idx:    0
			base_nodes:    base_nodes
			base_children: base_children
			node_start:    node_starts[0]
			child_start:   child_starts[0]
			struct_decls:  struct_decls
			sum_decls:     sum_decls
		}
		for ci in 1 .. n_jobs {
			mut view := shared_region_view(t.a, node_starts[ci], node_starts[ci + 1],
				child_starts[ci], child_starts[ci + 1])
			view.specialized_fn_nodes = map[int]bool{}
			view.specialized_fn_modules = map[int]string{}
			view.specialized_fn_files = map[int]string{}
			mut wtc := t.tc.fork_for_parallel_transform(view)
			mut w := t.fork_worker(view, wtc)
			w.global_temp_counter = node_starts[ci]
			w.generic_fn_decls_cache = decls.clone()
			w.generic_fn_decls_ready = true
			w.generic_receiver_methods_by_name = t.generic_receiver_methods_by_name.clone()
			args[ci] = MonomorphChunkArgs{
				worker:        voidptr(w)
				claims:        claims
				worker_idx:    ci
				base_nodes:    base_nodes
				base_children: base_children
				node_start:    node_starts[ci]
				child_start:   child_starts[ci]
				struct_decls:  struct_decls
				sum_decls:     sum_decls
			}
		}

		// The caller can itself be a scoped worker sharing its parent's immutable
		// signature base. This algorithm moves and mutates those maps, so detach
		// once before taking ownership of their storage.
		t.ensure_private_signature_maps()
		mut master_tc := unsafe { &types.TypeChecker(voidptr(t.tc)) }
		master_tc.ensure_private_transform_signatures()
		mut shared_fn_ret_types := t.fn_ret_types.move()
		mut shared_receiver_index := t.receiver_method_suffix_index.move()
		t.parallel_monomorph_scan_nodes = []int{}
		t.parallel_monomorph_struct_specs = map[string]string{}
		t.parallel_monomorph_sum_specs = map[string]GenericSpecContext{}
		t.parallel_monomorph_scan_start = base_nodes
		t.fn_ret_types = shared_fn_ret_types.clone()
		t.receiver_method_suffix_index = shared_receiver_index.clone()
		original_nodes_cap := t.a.nodes.cap
		original_children_cap := t.a.children.cap
		original_nodes_data := t.a.nodes.data
		original_children_data := t.a.children.data
		unsafe {
			t.a.nodes.cap = node_starts[1]
			t.a.nodes.flags.set(.nogrow)
			t.a.children.cap = child_starts[1]
			t.a.children.flags.set(.nogrow)
		}
		mut tasks := []workers.Task{cap: n_jobs}
		for ci in 0 .. n_jobs {
			tasks << workers.Task{
				run:        monomorph_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0
			}
		}
		transform_worker_scope_leave(setup_scope)
		t.monomorph_profile('mono setup: ${time.ticks() - debug_started} ms')
		any_started := t.a.worker_pool.run(tasks)
		t.monomorph_profile('mono workers: ${time.ticks() - debug_started} ms')
		unsafe {
			if t.a.nodes.data == original_nodes_data {
				t.a.nodes.cap = original_nodes_cap
			}
			t.a.nodes.flags.clear(.nogrow)
			if t.a.children.data == original_children_data {
				t.a.children.cap = original_children_cap
			}
			t.a.children.flags.clear(.nogrow)
		}
		// Worker regions still alias the original backing arrays. Compact them in
		// place when every destination remains before the next unmerged shared
		// region. Uneven/private regions can violate that ordering, so retain the
		// private master copy as the safe fallback for those batches.
		mut merged_nodes_cap := base_nodes
		mut merged_children_cap := base_children
		for ci in 0 .. n_jobs {
			w := unsafe { &Transformer(args[ci].worker) }
			merged_nodes_cap += w.a.nodes.len - node_starts[ci]
			merged_children_cap += w.a.children.len - child_starts[ci]
		}
		merge_in_place := t.monomorph_regions_can_merge_in_place(args, node_starts, child_starts,
			original_nodes_data, original_children_data, original_nodes_cap, original_children_cap,
			base_nodes, base_children)
		if !merge_in_place && t.a.nodes.data == original_nodes_data {
			nodes := clone_monomorph_node_region(t.a.nodes, base_nodes, base_nodes,
				merged_nodes_cap)
			unsafe {
				t.a.nodes.flags.set(.nofree)
			}
			t.a.nodes = nodes
			t.a.file_node_ids = []int{}
		}
		if !merge_in_place && t.a.children.data == original_children_data {
			children := clone_monomorph_child_region(t.a.children, base_children, base_children,
				merged_children_cap)
			unsafe {
				t.a.children.flags.set(.nofree)
			}
			t.a.children = children
		}
		master_fn_ret_types := t.fn_ret_types.move()
		master_receiver_index := t.receiver_method_suffix_index.move()
		t.fn_ret_types = shared_fn_ret_types.move()
		t.receiver_method_suffix_index = shared_receiver_index.move()
		for name, ret in master_fn_ret_types {
			t.fn_ret_types[name] = ret
		}
		for name, receiver in master_receiver_index {
			t.receiver_method_suffix_index[name] = receiver
		}

		for ci in 0 .. n_jobs {
			// args[ci].worker is a &Transformer stored as a voidptr: the master `t`
			// itself for ci == 0 (round-tripped through voidptr(t) at setup), otherwise a
			// worker forked by fork_worker. worker_pool.run() above already joined every
			// chunk thread, so each worker is finished and its region fully written, and
			// the workers stay live for this merge (args owns them and their arenas back
			// the merged regions), so reinterpreting the pointer here is sound.
			mut w := unsafe { &Transformer(args[ci].worker) }
			t.monomorph_profile('mono worker ${ci}: ${args[ci].emitted_specs.len} specs, ${w.a.nodes.len - node_starts[ci]} nodes, ${w.a.children.len - child_starts[ci]} children')
			mut node_shift := 0
			if ci > 0 {
				node_shift = t.a.nodes.len - node_starts[ci]
				t.merge_worker_used_fns(w)
				t.merge_worker(w, []FnWorkItem{}, node_starts[ci], child_starts[ci], false)
				for key, boxed in w.interface_boxed_types {
					if boxed && key !in t.interface_boxed_types {
						t.interface_boxed_types[key.clone()] = true
					}
				}
				for name in w.generic_specialization_args_log {
					spec_args := w.generic_specialization_args[name] or { continue }
					if name !in t.generic_specialization_args {
						t.generic_specialization_args[name.clone()] = spec_args.clone()
					}
				}
			}
			for idx in args[ci].scan_nodes {
				t.parallel_monomorph_scan_nodes << idx + node_shift
			}
			for spec, base in args[ci].struct_specs {
				t.parallel_monomorph_struct_specs[spec.clone()] = base.clone()
			}
			for spec, context in args[ci].sum_specs {
				t.parallel_monomorph_sum_specs[spec.clone()] = GenericSpecContext{
					base:   context.base.clone()
					file:   context.file.clone()
					module: context.module.clone()
				}
			}
			t.ensure_node_context_map_capacity()
			for idx, spec in args[ci].emitted_specs {
				root := flat.NodeId(int(args[ci].roots[idx]) + node_shift)
				t.record_monomorph_cache_spec(spec.key, spec.decl.key, spec.decl.module, spec.args)
				if !t.generic_specialization_registered(spec.decl, spec.args) {
					value := specialized_generic_fn_value(spec.decl.node.value, spec.args)
					t.register_specialized_fn_signature_value(spec.decl, value, spec.args)
				}
				t.generic_fn_spec_nodes[spec.key.clone()] = root
				t.a.specialized_fn_nodes[int(root)] = true
				t.a.specialized_fn_modules[int(root)] = spec.decl.module
				t.a.specialized_fn_files[int(root)] = spec.decl.file
				t.mark_node_context(root, spec.decl.module, spec.decl.file)
				emitted[generic_fn_spec_key(spec.decl.key, spec.args)] = true
				t.pending_generic_fn_spec_keys.delete(spec.key)
			}
			for name in args[ci].generated {
				generated << name.clone()
			}
			if ci > 0 {
				for pending in w.pending_generic_fn_specs {
					mut owned_args := []string{cap: pending.args.len}
					for item in pending.args {
						owned_args << item.clone()
					}
					t.request_generic_fn_specialization(pending.decl, owned_args)
				}
			}
			t.monomorph_profile('mono merged worker ${ci}: ${time.ticks() - debug_started} ms')
		}
		for ci in 1 .. n_jobs {
			if args[ci].scope != unsafe { nil } {
				t.monomorph_worker_scopes << args[ci].scope
			}
		}
		t.parallel_monomorph_worker = false
		t.generic_signatures_pre_registered = false
		t.global_temp_counter = node_starts[n_jobs]
		t.parallel_monomorph_scan_end = t.a.nodes.len
		t.tc.unfreeze_type_cache_after_forks()
		t.parallel_monomorph_scan_nodes = t.parallel_monomorph_scan_nodes.clone()
		mut owned_struct_specs := map[string]string{}
		for spec, base in t.parallel_monomorph_struct_specs {
			owned_struct_specs[spec.clone()] = base.clone()
		}
		t.parallel_monomorph_struct_specs = owned_struct_specs.move()
		mut owned_sum_specs := map[string]GenericSpecContext{}
		for spec, context in t.parallel_monomorph_sum_specs {
			owned_sum_specs[spec.clone()] = GenericSpecContext{
				base:   context.base.clone()
				file:   context.file.clone()
				module: context.module.clone()
			}
		}
		t.parallel_monomorph_sum_specs = owned_sum_specs.move()
		t.monomorph_worker_scopes << setup_scope
		t.monomorph_profile('mono merge: ${time.ticks() - debug_started} ms')
		return any_started
	}
}

// run_scoped_monomorphize_specs emits a bounded number of specializations in a
// private AST/checker view, merges their persistent output, and releases all
// per-specialization scratch before continuing with the next batch.
fn (mut t Transformer) run_scoped_monomorphize_specs(specs []PendingGenericFnSpec, mut emitted map[string]bool, mut generated []string) bool {
	if specs.len == 0 {
		return false
	}
	// Workers treat declaration parameter metadata as immutable. Build the lazy
	// index in the parent arena before a scoped worker can grow its backing map.
	t.prepare_parallel_call_param_types()
	// A scoped transformer can itself be a fork whose signature tables still
	// point at its parent's read-only base. Detach before pre-registering the
	// batch, rather than mutating storage another worker may be reading.
	t.ensure_private_signature_maps()
	t.tc.ensure_private_transform_signatures()
	t.tc.freeze_type_cache_for_forks()
	defer {
		t.tc.unfreeze_type_cache_after_forks()
	}
	decls := t.cached_generic_fn_decls()
	mut start := 0
	for start < specs.len {
		end := if start + scoped_monomorph_batch_specs < specs.len {
			start + scoped_monomorph_batch_specs
		} else {
			specs.len
		}
		base_nodes := t.a.nodes.len
		base_children := t.a.children.len
		node_headroom := (end - start) * 8192 + 262144
		child_headroom := (end - start) * 10240 + 393216
		t.a.nodes.ensure_cap(base_nodes + node_headroom)
		t.a.children.ensure_cap(base_children + child_headroom)
		for spec in specs[start..end] {
			if !t.generic_specialization_registered(spec.decl, spec.args) {
				value := specialized_generic_fn_value(spec.decl.node.value, spec.args)
				t.register_specialized_fn_signature_value(spec.decl, value, spec.args)
			}
		}
		scope := transform_worker_scope_begin(true)
		mut wast := shared_region_view(t.a, base_nodes, t.a.nodes.cap, base_children,
			t.a.children.cap)
		wast.specialized_fn_nodes = map[int]bool{}
		mut wtc := t.tc.fork_for_parallel_transform(wast)
		wtc.ensure_private_transform_signatures()
		mut w := t.fork_worker(wast, wtc)
		w.fn_ret_types = t.fn_ret_types.clone()
		w.receiver_method_suffix_index = t.receiver_method_suffix_index.clone()
		w.signature_maps_shared = false
		w.generic_fn_decls_cache = decls.clone()
		w.generic_fn_decls_ready = true
		w.generic_receiver_methods_by_name = t.generic_receiver_methods_by_name.clone()
		w.parallel_monomorph_worker = true
		w.generic_signatures_pre_registered = false
		w.defer_nested_generic_emissions = true
		w.generic_clone_children.ensure_cap(65536)
		mut roots := []flat.NodeId{cap: end - start}
		mut emitted_specs := []PendingGenericFnSpec{cap: end - start}
		mut generated_names := []string{}
		for spec in specs[start..end] {
			if spec.key in t.generic_fn_spec_nodes {
				continue
			}
			root := w.emit_generic_fn_specialization(spec.decl, spec.args)
			generated_names << w.generated_fn_used_names(spec.decl, root, spec.args)
			roots << root
			emitted_specs << spec
		}
		w.worker_scope = scope
		transform_worker_scope_leave(scope)

		node_shift := t.a.nodes.len - base_nodes
		// The parent pre-registered every specialization in this batch above.
		// The worker must still register them privately while transforming so
		// result/error semantics resolve correctly, but those duplicate maps live
		// in `scope` and must not escape when the other worker results are merged.
		w.signature_maps_changed = false
		w.fn_ret_types_log = []string{}
		w.tc_signature_names_log = []string{}
		w.tc.discard_transform_signature_changes()
		t.merge_worker_used_fns(w)
		t.merge_worker(w, []FnWorkItem{}, base_nodes, base_children, false)
		for name in w.generic_specialization_args_log {
			spec_args := w.generic_specialization_args[name] or { continue }
			if name !in t.generic_specialization_args {
				t.generic_specialization_args[name.clone()] = spec_args.clone()
			}
		}
		for pending in w.pending_generic_fn_specs {
			mut owned_args := []string{cap: pending.args.len}
			for item in pending.args {
				owned_args << item.clone()
			}
			t.request_generic_fn_specialization(pending.decl, owned_args)
		}
		for idx, spec in emitted_specs {
			root := flat.NodeId(int(roots[idx]) + node_shift)
			if !t.generic_specialization_registered(spec.decl, spec.args) {
				value := specialized_generic_fn_value(spec.decl.node.value, spec.args)
				t.register_specialized_fn_signature_value(spec.decl, value, spec.args)
			}
			t.generic_fn_spec_nodes[spec.key.clone()] = root
			t.a.specialized_fn_nodes[int(root)] = true
			t.mark_node_context(root, spec.decl.module, spec.decl.file)
			emitted[generic_fn_spec_key(spec.decl.key, spec.args)] = true
			t.pending_generic_fn_spec_keys.delete(spec.key)
		}
		for name in generated_names {
			generated << name.clone()
		}
		if t.stage_scope != unsafe { nil } {
			parent_state := transform_stage_scope_suspend(t.stage_scope)
			for idx in 0 .. t.a.nodes.len {
				t.clone_scoped_worker_node(idx, scope)
			}
			transform_stage_scope_resume(t.stage_scope, parent_state)
		} else {
			for idx in 0 .. t.a.nodes.len {
				t.promote_scoped_node_to_current(idx, scope)
			}
		}
		transform_worker_scope_free(scope)
		start = end
	}
	return true
}

fn monomorph_spec_worker(key string, worker_count int) int {
	if worker_count <= 1 {
		return 0
	}
	mut hash := u32(2166136261)
	for c in key.bytes() {
		hash = (hash ^ u32(c)) * u32(16777619)
	}
	return int(hash % u32(worker_count))
}

fn monomorph_job_count(n_runtime_jobs int, n_specs int) int {
	if n_runtime_jobs <= 0 || n_specs <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_parallel_monomorph_jobs {
		n = max_parallel_monomorph_jobs
	}
	if n > n_specs {
		n = n_specs
	}
	return n
}

fn monomorph_job_limit(available_jobs int, _node_count int, configured_jobs int) int {
	if available_jobs <= 1 {
		return 1
	}
	if configured_jobs > 0 {
		return int_min(available_jobs, configured_jobs)
	}
	// Scoped generic-transform batches keep enough memory available for the
	// lower-latency four-way specialization path on large applications too.
	return int_min(available_jobs, 4)
}

// monomorph_regions_can_merge_in_place reports whether sequential leftward
// compaction can leave every not-yet-merged shared worker region untouched.
// Private fallback regions do not constrain the destination, but their output
// still counts toward the position at which the next region will be appended.
fn (t &Transformer) monomorph_regions_can_merge_in_place(args []MonomorphChunkArgs, node_starts []int, child_starts []int, original_nodes_data voidptr, original_children_data voidptr, original_nodes_cap int, original_children_cap int, base_nodes int, base_children int) bool {
	if t.a.nodes.data != original_nodes_data || t.a.children.data != original_children_data {
		return false
	}
	mut node_dest := base_nodes
	mut child_dest := base_children
	for ci in 0 .. args.len {
		w := unsafe { &Transformer(args[ci].worker) }
		node_dest += w.a.nodes.len - node_starts[ci]
		child_dest += w.a.children.len - child_starts[ci]
		if node_dest > original_nodes_cap || child_dest > original_children_cap {
			return false
		}
		for next in ci + 1 .. args.len {
			next_worker := unsafe { &Transformer(args[next].worker) }
			if next_worker.a.nodes.data == original_nodes_data {
				if node_dest > node_starts[next] {
					return false
				}
				break
			}
		}
		for next in ci + 1 .. args.len {
			next_worker := unsafe { &Transformer(args[next].worker) }
			if next_worker.a.children.data == original_children_data {
				if child_dest > child_starts[next] {
					return false
				}
				break
			}
		}
	}
	return true
}

fn (t &Transformer) monomorph_worker_has_headroom(spec PendingGenericFnSpec) bool {
	cost := i64(t.generic_decl_source_cost(spec.decl))
	node_headroom := i64(65536) + cost * 64
	child_headroom := i64(98304) + cost * 80
	return i64(t.a.nodes.len) + node_headroom <= i64(t.a.nodes.cap)
		&& i64(t.a.children.len) + child_headroom <= i64(t.a.children.cap)
}

fn (mut t Transformer) detach_monomorph_worker_region(base_nodes int, base_children int, node_start int, child_start int, spec PendingGenericFnSpec) {
	cost := i64(t.generic_decl_source_cost(spec.decl))
	node_headroom := int(i64(65536) + cost * 64)
	child_headroom := int(i64(98304) + cost * 80)
	node_cap := monomorph_private_region_cap(t.a.nodes.len, node_headroom)
	child_cap := monomorph_private_region_cap(t.a.children.len, child_headroom)
	mut nodes := clone_monomorph_node_region(t.a.nodes, base_nodes, node_start, node_cap)
	mut children := clone_monomorph_child_region(t.a.children, base_children, child_start,
		child_cap)
	unsafe {
		// These descriptors alias the master's allocation. Never release it when
		// replacing them; other workers can still be appending to their regions.
		t.a.nodes.flags.set(.nofree)
		t.a.children.flags.set(.nofree)
	}
	t.a.nodes = nodes
	t.a.file_node_ids = []int{}
	t.a.children = children
}

fn monomorph_private_region_cap(current_len int, headroom int) int {
	required := current_len + headroom
	grown := current_len + current_len / 2
	return if grown > required { grown } else { required }
}

fn clone_monomorph_node_region(source []flat.Node, base_count int, region_start int, new_cap int) []flat.Node {
	// The gap between the immutable base and this worker's region belongs to
	// other workers in shared storage. Keep it empty in the private copy: some
	// generic helpers scan the full AST and must not observe uninitialized nodes.
	mut result := []flat.Node{len: source.len, cap: new_cap}
	unsafe {
		if base_count > 0 {
			vmemcpy(result.data, source.data, base_count * int(sizeof(flat.Node)))
		}
		own_count := source.len - region_start
		if own_count > 0 {
			vmemcpy(&result[region_start], &source[region_start],
				own_count * int(sizeof(flat.Node)))
		}
	}
	return result
}

fn clone_monomorph_child_region(source []flat.NodeId, base_count int, region_start int, new_cap int) []flat.NodeId {
	mut result := []flat.NodeId{len: source.len, cap: new_cap}
	unsafe {
		if base_count > 0 {
			vmemcpy(result.data, source.data, base_count * int(sizeof(flat.NodeId)))
		}
		own_count := source.len - region_start
		if own_count > 0 {
			vmemcpy(&result[region_start], &source[region_start],
				own_count * int(sizeof(flat.NodeId)))
		}
	}
	return result
}

fn (t &Transformer) generic_decl_source_cost(decl GenericFnDecl) int {
	idx := int(decl.id)
	if idx < 0 || isnil(t.tc) || t.tc.top_level_idx.len == 0 {
		return int(decl.node.children_count) + 1
	}
	mut low := 0
	mut high := t.tc.top_level_idx.len
	for low < high {
		mid := (low + high) / 2
		if t.tc.top_level_idx[mid] < idx {
			low = mid + 1
		} else {
			high = mid
		}
	}
	previous := if low > 0 { t.tc.top_level_idx[low - 1] } else { -1 }
	return if idx > previous { idx - previous } else { int(decl.node.children_count) + 1 }
}

// collect_interface_boxed_types_parallel scans independent AST ranges with
// private checker context, then publishes only the small boxed-type set.
fn (mut t Transformer) collect_interface_boxed_types_parallel() bool {
	$if windows {
		return false
	} $else {
		if t.a.nodes.len < 4096 {
			return false
		}
		if isnil(t.a.worker_pool) {
			t.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		mut n_jobs := t.a.worker_pool.size() + 1
		if n_jobs > max_shared_transform_jobs {
			n_jobs = max_shared_transform_jobs
		}
		if n_jobs <= 1 {
			return false
		}
		mut bounds := []int{len: n_jobs + 1}
		for i in 0 .. n_jobs + 1 {
			bounds[i] = t.a.nodes.len * i / n_jobs
		}
		mut files := []string{len: n_jobs}
		mut modules := []string{len: n_jobs}
		mut next_bound := 0
		mut file := ''
		mut module_name := ''
		for idx, node in t.a.nodes {
			for next_bound < n_jobs && bounds[next_bound] == idx {
				files[next_bound] = file
				modules[next_bound] = module_name
				next_bound++
			}
			if node.kind == .file {
				file = node.value
				module_name = t.tc.file_modules[file] or { '' }
			} else if node.kind == .module_decl {
				module_name = node.value
			}
		}
		mut args := []InterfaceBoxScanArgs{len: n_jobs}
		mut tasks := []workers.Task{cap: n_jobs}
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		t.tc.freeze_type_cache_for_forks()
		for i in 0 .. n_jobs {
			args[i] = InterfaceBoxScanArgs{
				source: voidptr(t)
				start:  bounds[i]
				end:    bounds[i + 1]
				file:   files[i]
				module: modules[i]
			}
			tasks << workers.Task{
				run:        interface_box_scan_thread
				arg:        unsafe { voidptr(&args[i]) }
				force_sync: i == 0 || fail == 'transform:all' || fail == 'transform:interface:all'
					|| fail == 'transform:interface:${i - 1}'
			}
		}
		t.a.worker_pool.run(tasks)
		t.tc.unfreeze_type_cache_after_forks()
		mut boxed_types := map[string]bool{}
		for arg in args {
			scan := unsafe { &Transformer(arg.worker) }
			for key, value in scan.interface_boxed_types {
				if value {
					boxed_types[key.clone()] = true
				}
			}
			transform_worker_scope_free(arg.scope)
		}
		t.interface_boxed_types = boxed_types.move()
		t.interface_boxed_types_done = true
		t.interface_boxed_types_frozen = true
		return true
	}
}

// promote_scoped_node_to_current copies only fields owned by `scope`. The
// caller has already left the scratch scope, so clones land in its small result
// arena and survive until the master merges this worker.
@[direct_array_access]
fn (mut t Transformer) promote_scoped_node_to_current(idx int, scope voidptr) {
	if idx < 0 || idx >= t.a.nodes.len {
		return
	}
	mut node := unsafe { &t.a.nodes[idx] }
	if node.value.len > 0 && transform_scope_owns(scope, node.value.str) {
		node.value = t.promote_scoped_result_text(node.value)
	}
	if node.typ.len > 0 && transform_scope_owns(scope, node.typ.str) {
		node.typ = t.promote_scoped_result_text(node.typ)
	}
	if isnil(node.payload) {
		return
	}
	old_params := node.generic_params()
	if old_params.len == 0 {
		return
	}
	mut needs_owned_params := transform_scope_owns(scope, node.payload)
		|| transform_scope_owns(scope, old_params.data)
	if !needs_owned_params {
		for param in old_params {
			if param.len > 0 && transform_scope_owns(scope, param.str) {
				needs_owned_params = true
				break
			}
		}
	}
	if !needs_owned_params {
		return
	}
	mut params := []string{cap: old_params.len}
	for param in old_params {
		if param.len > 0 && transform_scope_owns(scope, param.str) {
			params << t.promote_scoped_result_text(param)
		} else {
			params << param
		}
	}
	node.set_generic_params(params)
}

fn (mut t Transformer) promote_scoped_result_text(value string) string {
	if value.len == 0 {
		return ''
	}
	// Shallow string copies share `.str`, so one batch promotes the same
	// instance thousands of times: a pointer probe is much cheaper than the
	// content-hash table lookups below. Only valid within one absorb pass
	// (absorb_scoped_batch bumps the generation before scope addresses can be
	// reused).
	mut cache := t.promote_text_cache
	use_cache := !isnil(cache) && cache.active
	mut slot := 0
	if use_cache {
		slot = int((u64(voidptr(value.str)) >> 4) & 2047)
		// The length check guards against distinct strings sharing one base
		// pointer (unsafe zero-copy slices).
		if cache.generations[slot] == cache.generation && cache.ptrs[slot] == voidptr(value.str)
			&& cache.results[slot].len == value.len {
			return cache.results[slot]
		}
	}
	// Scoped workers only read the compilation text table. Reuse its canonical
	// strings before adding a worker-local entry; generic specialization clones
	// otherwise retain another copy of almost every source identifier and type.
	mut canonical := ''
	if promoted := t.scoped_promoted_texts[value] {
		canonical = promoted
	} else if id := t.a.text_ids[value] {
		canonical = t.a.text_values[int(id) - 1]
	} else {
		canonical = value.clone()
		t.scoped_promoted_texts[canonical] = canonical
	}
	if use_cache {
		cache.ptrs[slot] = voidptr(value.str)
		cache.generations[slot] = cache.generation
		cache.results[slot] = canonical
	}
	return canonical
}

// promote_scoped_ast_storage moves array backing that grew inside `scope` into
// the parent arena. Individual node payloads are promoted by absorb_scoped_batch;
// this preserves the flat containers themselves before the scratch arena dies.
fn (mut t Transformer) promote_scoped_ast_storage(scope voidptr) {
	owns_nodes := transform_scope_owns(scope, t.a.nodes.data)
	owns_children := transform_scope_owns(scope, t.a.children.data)
	if !isnil(t.tc) && t.tc.verbose {
		eprintln('  [leakcheck] pas: nodes ${u64(t.a.nodes.data)}/${t.a.nodes.len}/${t.a.nodes.cap} owned ${owns_nodes} children ${u64(t.a.children.data)}/${t.a.children.len}/${t.a.children.cap} owned ${owns_children}')
	}
	if owns_nodes {
		mut nodes := []flat.Node{cap: t.a.nodes.len + t.a.nodes.len / 4 + 1024}
		nodes << t.a.nodes
		t.a.nodes = nodes
		t.a.file_node_ids = []int{}
	}
	if owns_children {
		mut children := []flat.NodeId{cap: t.a.children.len + t.a.children.len / 4 + 1024}
		children << t.a.children
		t.a.children = children
	}
}

// absorb_scoped_batch publishes one batch's observable state into the helper's
// result arena before its large scratch arena is released.
fn (mut t Transformer) absorb_scoped_batch(batch &Transformer, scope voidptr, new_node_start int) {
	t.begin_promote_text_window()
	defer {
		t.end_promote_text_window()
	}
	t.merge_worker_signatures(batch)
	if t.skip_generics {
		// Non-generic workers mutate only newly appended nodes and slots recorded by
		// the setter log. This avoids a full, growing-AST scan after every batch.
		for idx in new_node_start .. batch.a.nodes.len {
			t.promote_scoped_node_to_current(idx, scope)
		}
		for idx in batch.scoped_owned_base_nodes.keys() {
			t.promote_scoped_node_to_current(idx, scope)
		}
		for idx in batch.scoped_owned_base_log {
			t.promote_scoped_node_to_current(idx, scope)
		}
	} else {
		// Generic lowering can rewrite nodes reached indirectly through late calls.
		// Keep the exhaustive fallback for those transforms.
		for idx in 0 .. batch.a.nodes.len {
			t.promote_scoped_node_to_current(idx, scope)
		}
	}
	for idx in batch.scoped_owned_base_nodes.keys() {
		t.scoped_owned_base_log << idx
	}
	t.scoped_owned_base_log << batch.scoped_owned_base_log
	t.inplace_child_log << batch.inplace_child_log
	for name in batch.used_fns_log {
		t.mark_used_fn_key(t.promote_scoped_result_text(name))
	}
	for name, used in batch.used_struct_operator_fns {
		if used && name !in t.used_struct_operator_fns {
			t.used_struct_operator_fns[t.promote_scoped_result_text(name)] = true
		}
	}
	for name, req in batch.sum_eq_types {
		if name !in t.sum_eq_types {
			t.sum_eq_types[t.promote_scoped_result_text(name)] = SumEqRequest{
				sum_name:      t.promote_scoped_result_text(req.sum_name)
				module:        t.promote_scoped_result_text(req.module)
				file:          t.promote_scoped_result_text(req.file)
				helper_module: t.promote_scoped_result_text(req.helper_module)
			}
		}
	}
	for name, req in batch.auto_str_types {
		if name !in t.auto_str_types {
			t.auto_str_types[t.promote_scoped_result_text(name)] = AutoStrRequest{
				module:        t.promote_scoped_result_text(req.module)
				file:          t.promote_scoped_result_text(req.file)
				helper_module: t.promote_scoped_result_text(req.helper_module)
			}
		}
	}
	for name, req in batch.default_clone_types {
		if name !in t.default_clone_types {
			t.default_clone_types[t.promote_scoped_result_text(name)] = DefaultCloneRequest{
				module: t.promote_scoped_result_text(req.module)
				file:   t.promote_scoped_result_text(req.file)
			}
		}
	}
	for message in batch.monomorph_errors {
		t.monomorph_errors << t.promote_scoped_result_text(message)
	}
	deferred_start := t.deferred_base_writes.len
	for write in batch.deferred_base_writes {
		t.deferred_base_writes << write
	}
	t.clone_deferred_worker_writes_from(deferred_start)
	if !isnil(batch.tc.fork_overlay) {
		for idx, name in batch.tc.fork_overlay.resolved_call_names {
			owned_name := t.promote_scoped_result_text(name)
			if isnil(t.tc.fork_overlay) {
				t.set_resolved_call_entry(idx, owned_name)
			} else {
				t.tc.fork_overlay.resolved_call_names[idx] = owned_name
			}
		}
		for idx, name in batch.tc.fork_overlay.resolved_fn_values {
			owned_name := t.promote_scoped_result_text(name)
			if isnil(t.tc.fork_overlay) {
				t.set_resolved_fn_value_entry(idx, owned_name)
			} else {
				t.tc.fork_overlay.resolved_fn_values[idx] = owned_name
			}
		}
	}
	if batch.ignored_comptime_for_nodes.len > 0 {
		for idx, ignored in batch.ignored_comptime_for_nodes {
			if ignored {
				t.ignored_comptime_for_log << idx
			}
		}
	}
	t.ignored_comptime_for_log << batch.ignored_comptime_for_log
}

// transform_scoped_helper_batches keeps one worker-pool dispatch but bounds
// scratch lifetime within each helper. A fresh Transformer/TypeChecker fork per
// batch prevents caches from retaining pointers into the released arena.
fn (mut t Transformer) transform_scoped_helper_batches(items []FnWorkItem, max_batches int) {
	t.retain_current_worker_scope_all()
	// NOTE (2026-08): a worker-persistent per-file normalize-result base shared
	// across batches was implemented and measured an exact wash — items are
	// sorted by fn_idx, so each file's work is contiguous within one worker and
	// almost never spans batches; there is no cross-batch refill churn to save.
	mut total_cost := i64(0)
	for item in items {
		total_cost += i64(item.cost) + 1
	}
	target_batches := if items.len < max_batches {
		items.len
	} else {
		max_batches
	}
	target_cost := if target_batches > 0 {
		(total_cost + i64(target_batches) - 1) / i64(target_batches)
	} else {
		i64(1)
	}
	mut start := 0
	mut batch_idx := 0
	for start < items.len {
		mut end := start
		mut batch_cost := i64(0)
		for end < items.len && end - start < scoped_transform_max_batch_items
			&& (batch_cost < target_cost || end == start) {
			batch_cost += i64(items[end].cost) + 1
			end++
		}
		text_start := t.a.text_values.len
		scratch_scope := transform_worker_scope_begin(true)
		batch_tc := t.tc.fork_for_parallel_transform(t.a)
		mut batch := t.fork_scoped_batch_worker(t.a, batch_tc)
		batch.used_fns_log_active = true
		batch.scoped_base_log_active = true
		batch.ignored_comptime_log_active = true
		new_node_start := t.a.nodes.len
		// Nodes appended by an earlier batch are base nodes for this batch too.
		// Record rewrites to them so their scratch-owned payloads are promoted.
		batch.scoped_base_nodes = new_node_start
		batch.transform_pure_items_serial(items[start..end])
		transform_worker_scope_leave(scratch_scope)
		t.a.promote_transform_texts_from(text_start, scratch_scope)
		t.absorb_scoped_batch(batch, scratch_scope, new_node_start)
		t.promote_scoped_ast_storage(scratch_scope)
		for item in items[start..end] {
			if item.fn_idx >= 0 && item.fn_idx < t.transformed_fns.len {
				t.transformed_fns[item.fn_idx] = true
			}
		}
		// absorb_scoped_batch publishes every appended node and every base-node
		// mutation recorded by the batch. Avoid rescanning the continuously growing
		// AST after each small batch; that makes scoped transform quadratic.
		transform_worker_scope_free(scratch_scope)
		start = end
		batch_idx++
	}
	// Promotions and merged side tables were allocated in the helper thread's
	// persistent parent arena after each scratch scope was left. Nothing from a
	// completed helper needs to keep a result arena alive until the end of the
	// whole transform phase.
	t.worker_scope = unsafe { nil }
}

// transform_late_candidates_scoped lowers dynamically discovered function bodies in bounded
// scratch arenas. The dependency queue and candidate index stay in the parent arena; only the
// allocation-heavy per-function transformer forks are discarded after each batch.
fn (mut t Transformer) transform_late_candidates_scoped(candidate_index map[string][]int, mut candidates []LateFnCandidate, mut late map[string]bool, mut pending []string, mut queued map[string]bool) {
	for pending.len > 0 {
		mut selected := []int{cap: scoped_transform_max_batch_items}
		for pending.len > 0 && selected.len < scoped_transform_max_batch_items {
			name := pending.pop()
			spellings := [name, c_name(name)]
			mut retry_name := false
			for key in spellings {
				for ci in candidate_index[key] {
					if candidates[ci].processed {
						continue
					}
					node := t.a.nodes[candidates[ci].idx]
					if !late_used_fn_matches(late, node, candidates[ci].module) {
						continue
					}
					candidates[ci].processed = true
					selected << ci
					if selected.len == scoped_transform_max_batch_items {
						retry_name = true
						break
					}
				}
				if retry_name {
					break
				}
			}
			if retry_name {
				pending << name
			}
		}
		if selected.len == 0 {
			continue
		}
		log_start := t.used_fns_log.len
		mut node_starts := []int{len: selected.len + 1}
		text_start := t.a.text_values.len
		scratch_scope := transform_worker_scope_begin(true)
		batch_tc := t.tc.fork_for_parallel_transform(t.a)
		mut batch := t.fork_scoped_batch_worker(t.a, batch_tc)
		batch.used_fns_log_active = true
		batch.scoped_base_log_active = true
		batch.ignored_comptime_log_active = true
		new_node_start := t.a.nodes.len
		batch.scoped_base_nodes = new_node_start
		for si, ci in selected {
			node_starts[si] = t.a.nodes.len
			batch.cur_file = candidates[ci].file
			batch.cur_module = candidates[ci].module
			batch.transform_fn_body(candidates[ci].idx)
		}
		node_starts[selected.len] = t.a.nodes.len
		transform_worker_scope_leave(scratch_scope)
		t.a.promote_transform_texts_from(text_start, scratch_scope)
		t.absorb_scoped_batch(batch, scratch_scope, new_node_start)
		t.promote_scoped_ast_storage(scratch_scope)
		transform_worker_scope_free(scratch_scope)
		for si, ci in selected {
			idx := candidates[ci].idx
			if idx >= 0 && idx < t.transformed_fns.len {
				t.transformed_fns[idx] = true
			}
			t.cur_file = candidates[ci].file
			t.cur_module = candidates[ci].module
			for call_name in t.generated_fn_body_call_names(flat.NodeId(idx)) {
				t.enqueue_late_used_call_name(call_name, log_start, mut late, mut pending, mut
					queued)
			}
			for j in node_starts[si] .. node_starts[si + 1] {
				generated := t.a.nodes[j]
				if generated.kind != .fn_decl
					|| !transform_is_generated_fn_after_markused(generated.value) {
					continue
				}
				for call_name in t.generated_fn_body_call_names(flat.NodeId(j)) {
					t.enqueue_late_used_call_name(call_name, log_start, mut late, mut pending, mut
						queued)
				}
			}
		}
	}
}

// clone_deferred_worker_writes_from moves writes queued by merge_worker out of
// a helper arena before that arena is released. In the shared-base path the
// rewritten top-level fn node is deferred until every worker has joined, so
// cloning the current master slot alone does not preserve its owned strings.
fn (mut t Transformer) clone_deferred_worker_writes_from(start int) {
	for i in start .. t.deferred_base_writes.len {
		write := t.deferred_base_writes[i]
		t.deferred_base_writes[i] = match write.kind {
			0, 1 {
				DeferredBaseWrite{
					idx:  write.idx
					kind: write.kind
					str:  t.promote_scoped_result_text(write.str)
				}
			}
			2 {
				mut params := []string{cap: write.node.generic_params().len}
				for param in write.node.generic_params() {
					params << t.promote_scoped_result_text(param)
				}
				DeferredBaseWrite{
					idx:  write.idx
					kind: write.kind
					node: flat.Node{
						value:                t.promote_scoped_result_text(write.node.value)
						typ:                  t.promote_scoped_result_text(write.node.typ)
						payload:              flat.node_payload(params)
						pos:                  write.node.pos
						children_start:       write.node.children_start
						children_count:       write.node.children_count
						kind:                 write.node.kind
						op:                   write.node.op
						is_mut:               write.node.is_mut
						skip_ownership_drops: write.node.skip_ownership_drops
					}
				}
			}
			else {
				mut params := []string{cap: write.gparams.len}
				for param in write.gparams {
					params << t.promote_scoped_result_text(param)
				}
				DeferredBaseWrite{
					idx:     write.idx
					kind:    write.kind
					gparams: params
				}
			}
		}
	}
}

// run_parallel_transform transforms the closure-free function bodies across
// threads when there is enough work, otherwise serially. Returns whether threads
// were actually used.
fn (mut t Transformer) run_parallel_transform(items []FnWorkItem, base_nodes int, base_children int) bool {
	$if windows {
		t.transform_pure_items_serial(items)
		return false
	} $else {
		// Generic body lowering can discover signatures, so generic builds use the
		// cloned-worker path below. Each worker owns its signature maps and the
		// deterministic merge publishes additions after all body work has joined.
		if isnil(t.a.worker_pool) {
			t.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		n_jobs := transform_job_count(t.a.worker_pool.size() + 1, items.len)
		if items.len < min_parallel_transform_items || n_jobs <= 1 {
			t.transform_pure_items_serial(items)
			return false
		}
		// Workers need declaration signatures while lowering calls. Snapshot them
		// before any worker can rewrite a shared-base fn_decl; lazily scanning or
		// reading declarations inside workers can otherwise observe a torn node.
		mut prep_sw := time.new_stopwatch()
		t.prepare_parallel_call_param_types()
		t.timing_profile('  [ttime]   prep param types ${f64(prep_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		// Clone-free shared-base path: needs the checker's top-level index for
		// exact per-item subtree ranges, and skip_generics (the generic passes
		// scan and mutate arbitrary AST regions, which the shared design forbids).
		if t.skip_generics && !isnil(t.tc) && t.tc.top_level_idx.len > 0 {
			shared_jobs := shared_transform_job_count(t.a.worker_pool.size() + 1, items.len)
			if shared_jobs > 1 {
				return t.run_parallel_transform_shared(items, base_nodes, base_children,
					shared_jobs)
			}
		}
		// Freeze the checker's warm type cache (fully populated by the check
		// phase) as the shared read-only base for every worker fork, so workers
		// do not re-parse every type text from a cold cache; the master itself
		// writes through a private overlay for the duration of the region.
		t.tc.freeze_type_cache_for_forks()
		mut chunks := split_work_items(items, n_jobs)
		chunk_count := chunks.len

		// chunk[0] is transformed by the master on this thread, directly against the
		// master AST — no clone. Only chunks[1..] get helper threads, each with a
		// private AST clone + forked TypeChecker. This removes one full base-AST clone
		// from the peak (each clone is ~one nodes-array; under -gc none they are never
		// freed, so they also inflate the later cgen peak) and keeps the master thread,
		// which would otherwise block in join, doing useful work.
		thread_count := chunk_count - 1
		mut transform_workers := []voidptr{cap: thread_count}
		mut args := []TransformChunkArgs{cap: chunk_count}
		args << TransformChunkArgs{
			worker:    voidptr(t)
			items_ptr: unsafe { voidptr(&chunks[0]) }
		}
		for ci in 0 .. thread_count {
			wast := t.clone_ast_base(base_nodes, base_children)
			wtc := t.tc.fork_for_parallel_transform(wast)
			ww := t.fork_worker(wast, wtc)
			transform_workers << voidptr(ww)
			args << TransformChunkArgs{
				worker:    voidptr(ww)
				items_ptr: unsafe { voidptr(&chunks[ci + 1]) }
			}
		}
		// Helper forks share the current signature and struct-map backing arrays.
		// The master participates as chunk 0, so make its next metadata write detach
		// instead of reallocating storage while helpers are reading it.
		t.mark_parallel_worker_maps_shared()
		t.temp_counter = 0
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: chunk_count}
		for ci in 0 .. chunk_count {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        transform_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'transform:all' || fail == 'transform:${helper_idx}'
			}
		}
		any_started := t.a.worker_pool.run(tasks)
		// Merge each helper in fixed chunk order for deterministic node ids.
		for ci in 0 .. thread_count {
			ww := unsafe { &Transformer(transform_workers[ci]) }
			t.merge_worker_used_fns(ww)
			t.merge_worker(ww, chunks[ci + 1], base_nodes, base_children, true)
		}
		t.tc.unfreeze_type_cache_after_forks()
		return any_started
	}
}

fn (mut t Transformer) mark_parallel_worker_maps_shared() {
	t.signature_maps_shared = true
	t.struct_maps_shared = true
	if !isnil(t.tc) {
		mut master_tc := unsafe { &types.TypeChecker(voidptr(t.tc)) }
		master_tc.transform_signature_maps_shared = true
		master_tc.transform_struct_maps_shared = true
	}
}

// shared_transform_job_count caps the shared-base worker count: no clones, so
// only core count and item count matter.
fn shared_transform_job_count(n_runtime_jobs int, n_items int) int {
	if n_runtime_jobs <= 0 || n_items <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_shared_transform_jobs {
		n = max_shared_transform_jobs
	}
	if n > n_items {
		n = n_items
	}
	return n
}

// shared_region_view builds a FlatAst whose arrays alias the master's data but
// whose len/cap bound this worker's private append region [nstart, nend) /
// [cstart, cend). Base reads (ids below nstart) see the shared immutable base;
// appends land in the region, so the new node ids are already master-global.
// .nogrow turns a region overflow into a loud panic instead of a realloc that
// would free the master's live block.
fn shared_region_view(a &flat.FlatAst, nstart int, nend int, cstart int, cend int) &flat.FlatAst {
	mut nodes := []flat.Node{}
	mut children := []flat.NodeId{}
	unsafe {
		nodes.data = a.nodes.data
		nodes.len = nstart
		nodes.cap = nend
		nodes.flags.set(.nogrow)
		children.data = a.children.data
		children.len = cstart
		children.cap = cend
		children.flags.set(.nogrow)
	}
	return &flat.FlatAst{
		nodes:                  nodes
		children:               children
		user_code_start:        a.user_code_start
		disabled_fns:           a.disabled_fns
		noreturn_fns:           a.noreturn_fns
		source_files:           a.source_files
		template_call_sites:    a.template_call_sites
		template_actions:       a.template_actions
		source_buffers:         a.source_buffers
		text_values:            a.text_values
		text_ids:               a.text_ids
		worker_pool:            a.worker_pool
		specialized_fn_nodes:   a.specialized_fn_nodes.clone()
		specialized_fn_modules: a.specialized_fn_modules.clone()
		specialized_fn_files:   a.specialized_fn_files.clone()
	}
}

// bound_shared_expansion keeps the combined map and interpolation lowering
// estimate within the shared append pool. Individually bounded expansions can
// still exhaust the fixed .nogrow regions when many functions contain them.
fn (mut t Transformer) bound_shared_expansion(items []FnWorkItem, node_pool int, child_pool int) []FnWorkItem {
	mut pool := node_pool
	if child_pool < pool {
		pool = child_pool
	}
	if pool < 0 {
		pool = 0
	}
	budget := pool / shared_expansion_pool_divisor
	mut used := 0
	mut bounded := []FnWorkItem{cap: items.len}
	mut deferred_any := false
	for item in items {
		estimate := item.map_expansion_estimate + item.interp_expansion_estimate
		if estimate > 0 && estimate > budget - used {
			t.deferred_expansion_items << item
			deferred_any = true
			continue
		}
		used += estimate
		bounded << item
	}
	if deferred_any {
		t.deferred_expansion_items.sort(a.fn_idx < b.fn_idx)
	}
	return bounded
}

// run_parallel_transform_shared is the clone-free variant of the parallel
// transform: all threads (master included) work directly on the master arrays.
// Fn subtrees are disjoint node ranges, transform only rewrites nodes inside
// the fn currently being lowered (plus appends), and stray out-of-range writes
// are intercepted (see base_write_allowed), so threads never touch each
// other's slots. Each thread appends into its own pre-partitioned capacity
// region; the master then compacts the regions into the final sequential
// layout with the same shift arithmetic merge_worker always used (a region
// start plays the role the clone base played).
fn (mut t Transformer) run_parallel_transform_shared(items []FnWorkItem, base_nodes int, base_children int, n_jobs int) bool {
	$if windows {
		t.transform_pure_items_serial(items)
		return false
	} $else {
		mut ttsw := time.new_stopwatch()
		node_pool := t.a.nodes.cap - base_nodes
		child_pool := t.a.children.cap - base_children
		bounded_items := t.bound_shared_expansion(items, node_pool, child_pool)
		if bounded_items.len < min_parallel_transform_items {
			t.transform_pure_items_serial(bounded_items)
			return false
		}
		t.tc.freeze_type_cache_for_forks()
		mut chunk_target := n_jobs * shared_transform_chunks_per_job
		if chunk_target > bounded_items.len {
			chunk_target = bounded_items.len
		}
		mut chunks := split_work_items_ex(bounded_items, chunk_target, false)
		chunk_count := chunks.len
		thread_count := chunk_count - 1
		// Pool.run queues asynchronous work before running synchronous tasks. Give
		// the caller the same number of chunks as every persistent worker so its
		// core remains useful across both scheduling waves.
		mut sync_chunk_count := chunk_count - (n_jobs - 1) * shared_transform_chunks_per_job
		if sync_chunk_count < 1 {
			sync_chunk_count = 1
		}
		// Partition the reserved capacity into per-chunk append regions,
		// proportional to chunk cost (the caller reserved ~2x the expected
		// total growth for this pool).
		mut costs := []i64{len: chunk_count}
		mut total := i64(0)
		for ci in 0 .. chunk_count {
			for it in chunks[ci] {
				costs[ci] += i64(it.cost) + 1
			}
			total += costs[ci]
		}
		if total <= 0 {
			total = 1
		}
		mut node_starts := []int{len: chunk_count + 1}
		mut child_starts := []int{len: chunk_count + 1}
		node_starts[0] = base_nodes
		child_starts[0] = base_children
		mut acc := i64(0)
		for ci in 0 .. chunk_count {
			acc += costs[ci]
			node_starts[ci + 1] = base_nodes + int(i64(node_pool) * acc / total)
			child_starts[ci + 1] = base_children + int(i64(child_pool) * acc / total)
		}
		t.base_write_intercept = true
		t.defer_oor_writes = true
		t.shared_base_nodes = base_nodes
		t.shared_base_children = base_children
		t.node_context_read_only = true
		t.timing_profile('  [ttime]     ss split+part  ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		setup_scope := transform_worker_scope_begin(t.scope_parallel_workers)
		mut args := []SharedChunkArgs{len: chunk_count}
		args[0] = SharedChunkArgs{
			worker:    voidptr(t)
			items_ptr: unsafe { voidptr(&chunks[0]) }
			is_master: true
		}
		mut sfsw := time.new_stopwatch()
		for ci in 0 .. thread_count {
			view := shared_region_view(t.a, node_starts[ci + 1], node_starts[ci + 2], child_starts[
				ci + 1], child_starts[ci + 2])
			view_ms := f64(sfsw.elapsed().microseconds()) / 1000.0
			wtc := t.tc.fork_for_parallel_transform(view)
			tc_ms := f64(sfsw.elapsed().microseconds()) / 1000.0
			mut ww := t.fork_worker(view, wtc)
			ww.defer_oor_writes = false
			args[ci + 1] = SharedChunkArgs{
				worker:    voidptr(ww)
				items_ptr: unsafe { voidptr(&chunks[ci + 1]) }
			}
			t.timing_profile('  [ttime]     ss fork ${ci} view ${view_ms:.2f} tc ${tc_ms - view_ms:.2f} wk ${f64(sfsw.elapsed().microseconds()) / 1000.0 - tc_ms:.2f} ms')
			sfsw.restart()
		}
		// The master takes region 0, which is [base, node_starts[1]) — exactly
		// where compaction wants its output, so its appends need no shifting
		// and its checker-cache writes use final node ids. Bound its arrays so
		// an overflow panics instead of reallocating the shared block away
		// from under the workers.
		orig_nodes_cap := t.a.nodes.cap
		orig_children_cap := t.a.children.cap
		unsafe {
			t.a.nodes.cap = node_starts[1]
			t.a.nodes.flags.set(.nogrow)
			t.a.children.cap = child_starts[1]
			t.a.children.flags.set(.nogrow)
		}
		t.temp_counter = 0
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: chunk_count}
		for ci in 0 .. chunk_count {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        shared_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci < sync_chunk_count || fail == 'transform:all'
					|| fail == 'transform:${helper_idx}'
			}
		}
		transform_worker_scope_leave(setup_scope)
		t.timing_profile('  [ttime]   shared setup     ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms (chunks: ${chunk_count}, jobs: ${n_jobs})')
		ttsw.restart()
		any_started := t.a.worker_pool.run(tasks)
		t.node_context_read_only = false
		t.timing_profile('  [ttime]   shared pool.run  ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		$if v3_ttime ? {
			for ci, arg in args {
				items_arg := unsafe { &[]FnWorkItem(arg.items_ptr) }
				t.timing_profile('  [ttime]     chunk ${ci:2} items=${items_arg.len:4} cost=${arg.cost:8} master=${arg.is_master} ${f64(arg.elapsed_us) / 1000.0:7.2f} ms')
			}
		}
		ttsw.restart()
		unsafe {
			t.a.nodes.cap = orig_nodes_cap
			t.a.nodes.flags.clear(.nogrow)
			t.a.children.cap = orig_children_cap
			t.a.children.flags.clear(.nogrow)
		}
		// The master chunk can publish its scoped batches eagerly, which clears
		// worker_scope before this merge. Its bodies were still transformed and
		// must be excluded from the late-use pass just like helper chunks.
		for item in chunks[0] {
			if item.fn_idx >= 0 && item.fn_idx < t.transformed_fns.len {
				t.transformed_fns[item.fn_idx] = true
			}
		}
		if t.retain_worker_results && t.worker_scope != unsafe { nil } {
			mut master_base_nodes := t.scoped_owned_base_nodes.keys()
			master_base_nodes << t.scoped_owned_base_log
			for write in t.deferred_base_writes {
				master_base_nodes << write.idx
			}
			t.retained_worker_regions << ScopedTransformRegion{
				scope:      t.worker_scope
				new_start:  base_nodes
				new_end:    t.a.nodes.len
				base_nodes: master_base_nodes
			}
		}
		// Relocate every worker region's appended ids in place first, in
		// parallel: the shifts derive from the deterministic region content
		// lengths, so the serial compaction below degrades to plain memmoves.
		// Only valid when compaction will not re-clone nodes (clone_worker_nodes
		// in merge_worker re-reads children_start values).
		if thread_count > 0 && (t.retain_worker_results || t.stage_scope != unsafe { nil })
			&& os.getenv('V3_NO_MERGE_RELOC').len == 0 {
			mut running_nodes := t.a.nodes.len
			mut running_children := t.a.children.len
			mut reloc_args := []RegionRelocateArgs{cap: thread_count}
			for ci in 0 .. thread_count {
				ww := unsafe { &Transformer(args[ci + 1].worker) }
				ns := node_starts[ci + 1]
				cs := child_starts[ci + 1]
				reloc_args << RegionRelocateArgs{
					worker:      args[ci + 1].worker
					node_start:  ns
					node_end:    ww.a.nodes.len
					child_start: cs
					child_end:   ww.a.children.len
					node_shift:  i32(running_nodes - ns)
					child_shift: i32(running_children - cs)
				}
				running_nodes += ww.a.nodes.len - ns
				running_children += ww.a.children.len - cs
			}
			mut reloc_tasks := []workers.Task{cap: thread_count}
			for i in 0 .. reloc_args.len {
				reloc_tasks << workers.Task{
					run:        region_relocate_thread
					arg:        unsafe { voidptr(&reloc_args[i]) }
					force_sync: i == 0
				}
			}
			t.a.worker_pool.run(reloc_tasks)
			t.merge_regions_relocated = true
		}
		// Compact each worker region in fixed order (deterministic
		// node numbering). merge_worker treats the region start exactly like a
		// clone's base offset; compaction always moves content left, so the
		// copies never collide with unmerged regions.
		mut merge_used_ms := f64(0)
		mut merge_core_ms := f64(0)
		mut mwsw := time.new_stopwatch()
		for ci in 0 .. thread_count {
			ww := unsafe { &Transformer(args[ci + 1].worker) }
			t.timing_profile('  [ttime]     mg region ${ci}: nodes [${node_starts[ci + 1]}, ${ww.a.nodes.len}) cap ${node_starts[
				ci + 2]} children [${child_starts[ci + 1]}, ${ww.a.children.len}) cap ${child_starts[
				ci + 2]} dst n ${t.a.nodes.len} c ${t.a.children.len}')
			mwsw.restart()
			t.merge_worker_used_fns(ww)
			merge_used_ms += f64(mwsw.elapsed().microseconds()) / 1000.0
			deferred_start := t.deferred_base_writes.len
			merged_node_start := t.a.nodes.len
			mwsw.restart()
			// Compaction appends each worker at the current master end. Sparse cache
			// entries from the master and earlier workers end before that fresh range,
			// so clearing every new id would only hash absent keys.
			t.merge_worker(ww, chunks[ci + 1], node_starts[ci + 1], child_starts[ci + 1], false)
			merge_core_ms += f64(mwsw.elapsed().microseconds()) / 1000.0
			if ww.worker_scope != unsafe { nil } && !t.retain_worker_results {
				t.clone_deferred_worker_writes_from(deferred_start)
				transform_worker_scope_free(ww.worker_scope)
			} else if ww.worker_scope != unsafe { nil } {
				mut worker_base_nodes := ww.scoped_owned_base_nodes.keys()
				worker_base_nodes << ww.scoped_owned_base_log
				for item in chunks[ci + 1] {
					worker_base_nodes << item.fn_idx
				}
				for write in ww.deferred_base_writes {
					worker_base_nodes << write.idx
				}
				t.retained_worker_regions << ScopedTransformRegion{
					scope:      ww.worker_scope
					new_start:  merged_node_start
					new_end:    t.a.nodes.len
					base_nodes: worker_base_nodes
				}
			}
		}
		t.merge_regions_relocated = false
		t.timing_profile('  [ttime]   shared merge     ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms (used: ${merge_used_ms:.2f}, core: ${merge_core_ms:.2f})')
		ttsw.restart()
		if t.retain_worker_results {
			t.clone_sum_eq_types_owned()
			t.clone_auto_str_types_owned()
			t.clone_default_clone_types_owned()
		}
		t.base_write_intercept = false
		t.defer_oor_writes = false
		t.shared_base_nodes = -1
		t.shared_base_children = -1
		t.flush_deferred_base_writes()
		if t.ignored_comptime_for_log.len > 0 {
			if t.ignored_comptime_for_nodes.len < t.a.nodes.len {
				t.ignored_comptime_for_nodes.ensure_cap(t.a.nodes.cap)
				t.ignored_comptime_for_nodes << []bool{len: t.a.nodes.len - t.ignored_comptime_for_nodes.len}
			}
			for idx in t.ignored_comptime_for_log {
				if idx >= 0 && idx < t.ignored_comptime_for_nodes.len {
					t.ignored_comptime_for_nodes[idx] = true
				}
			}
		}
		t.tc.unfreeze_type_cache_after_forks()
		transform_worker_scope_free(setup_scope)
		t.timing_profile('  [ttime]   shared tail      ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		return any_started
	}
}

$if !windows {
	// LateScanChunkArgs is the payload handed to each late-name-scan worker.
	struct LateScanChunkArgs {
		worker          voidptr // &Transformer
		cands_ptr       voidptr // &[]LateFnCandidate
		used            &map[string]bool = unsafe { nil }
		candidate_names &map[string]bool = unsafe { nil }
		results_ptr     voidptr // &[][]string
		index           int
		start           int
		end             int
	}

	// late_scan_chunk_thread runs one worker's candidate range of the late-name scan.
	fn late_scan_chunk_thread(arg voidptr) voidptr {
		args := unsafe { &LateScanChunkArgs(arg) }
		mut w := unsafe { &Transformer(args.worker) }
		cands := unsafe { &[]LateFnCandidate(args.cands_ptr) }
		mut results := unsafe { &[][]string(args.results_ptr) }
		res := w.scan_late_call_names_range(*cands, args.used, args.candidate_names, args.start,
			args.end)
		unsafe {
			(*results)[args.index] = res
		}
		return unsafe { nil }
	}
}

// scan_late_call_names_dispatch runs the late-name scan across threads when
// there is enough work. The scan is read-only over the merged AST (each worker
// gets a forked TypeChecker for its private memoization), and the per-range
// results are concatenated in range order. The downstream late-work queue
// deduplicates matching names.
fn (mut t Transformer) scan_late_call_names_dispatch(cands []LateFnCandidate, used &map[string]bool, candidate_names &map[string]bool) []string {
	$if windows {
		return t.scan_late_call_names_range(cands, used, candidate_names, 0, cands.len)
	} $else {
		if !t.parallel_enabled {
			return t.scan_late_call_names_range(cands, used, candidate_names, 0, cands.len)
		}
		// The scan clones no ASTs (workers share the merged AST read-only), so it
		// is not bound by the clone-memory ceiling of the transform workers.
		if isnil(t.a.worker_pool) {
			t.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		mut n_jobs := t.a.worker_pool.size() + 1
		if n_jobs > 10 {
			n_jobs = 10
		}
		if n_jobs > cands.len {
			n_jobs = cands.len
		}
		if cands.len < 256 || n_jobs <= 1 {
			return t.scan_late_call_names_range(cands, used, candidate_names, 0, cands.len)
		}
		t.tc.freeze_type_cache_for_forks()
		bounds := late_scan_chunk_bounds(t.a, cands, n_jobs)
		thread_count := n_jobs - 1
		mut results := [][]string{len: n_jobs, init: []string{}}
		mut scan_workers := []voidptr{len: thread_count, init: unsafe { nil }}
		mut args := []LateScanChunkArgs{len: n_jobs}
		args[0] = LateScanChunkArgs{
			worker:          voidptr(t)
			cands_ptr:       unsafe { voidptr(&cands) }
			used:            unsafe { used }
			candidate_names: unsafe { candidate_names }
			results_ptr:     unsafe { voidptr(&results) }
			index:           0
			start:           bounds[0]
			end:             bounds[1]
		}
		for ci in 0 .. thread_count {
			// No AST clone: the scan never appends nodes. Only the checker is
			// forked (private type_cache) and the Transformer's per-function
			// context is private to the fork.
			wtc := t.tc.fork_for_parallel_transform(t.a)
			ww := t.fork_scan_worker(wtc)
			scan_workers[ci] = voidptr(ww)
			args[ci + 1] = LateScanChunkArgs{
				worker:          scan_workers[ci]
				cands_ptr:       unsafe { voidptr(&cands) }
				used:            unsafe { used }
				candidate_names: unsafe { candidate_names }
				results_ptr:     unsafe { voidptr(&results) }
				index:           ci + 1
				start:           bounds[ci + 1]
				end:             bounds[ci + 2]
			}
		}
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: n_jobs}
		for ci in 0 .. n_jobs {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        late_scan_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'transform:all' || fail == 'transform:${helper_idx}'
			}
		}
		t.a.worker_pool.run(tasks)
		t.tc.unfreeze_type_cache_after_forks()
		mut names := []string{}
		for res in results {
			for name in res {
				names << name
			}
		}
		return names
	}
}

// late_scan_chunk_bounds splits the candidate list into n contiguous ranges of
// roughly equal node count (the span to the next candidate approximates each
// body's subtree size).
fn late_scan_chunk_bounds(a &flat.FlatAst, cands []LateFnCandidate, n int) []int {
	mut total := i64(0)
	mut costs := []i64{cap: cands.len}
	for i, cand in cands {
		next := if i + 1 < cands.len { cands[i + 1].idx } else { a.nodes.len }
		cost := i64(next - cand.idx)
		costs << if cost > 0 { cost } else { i64(1) }
		total += costs[i]
	}
	mut bounds := []int{cap: n + 1}
	bounds << 0
	mut acc := i64(0)
	mut chunk := 1
	for i in 0 .. cands.len {
		acc += costs[i]
		if chunk < n && acc >= total * i64(chunk) / i64(n) {
			bounds << i + 1
			chunk++
		}
	}
	for bounds.len < n + 1 {
		bounds << cands.len
	}
	return bounds
}

// transform_job_count caps the worker count by both the runtime job count and a
// fixed ceiling (each worker clones the base AST, so more workers cost more memory).
fn transform_job_count(n_runtime_jobs int, n_items int) int {
	if n_runtime_jobs <= 0 || n_items <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_parallel_transform_jobs {
		n = max_parallel_transform_jobs
	}
	if n > n_items {
		n = n_items
	}
	return n
}

// prepare_with_pre_scans runs prepare() while the const-array fixed-storage
// classification (a full post-markused AST pass) proceeds on a helper thread.
// The scan publishes into the master cache before this function returns, so
// transform_all_dispatch's precompute call becomes a no-op.
fn (mut t Transformer) prepare_with_pre_scans() {
	$if windows {
		t.prepare()
		return
	} $else {
		if !t.parallel_enabled || !t.skip_generics || !t.scope_parallel_workers || isnil(t.tc) {
			t.prepare()
			return
		}
		// The scan worker shares only immutable state: the AST (stable while the
		// master builds its indexes; any reserve/grow runs after this returns)
		// and the checker's post-check const/import tables. Its suffix map and
		// result cache are private and cloned out below.
		mut w := &Transformer{
			a:                               t.a
			tc:                              t.tc
			skip_generics:                   t.skip_generics
			building_v:                      t.building_v
			scope_parallel_workers:          t.scope_parallel_workers
			const_suffixes:                  map[string]string{}
			const_array_fixed_storage_cache: map[string]i8{}
		}
		scan_thread := spawn transform_const_fixed_scan_thread(voidptr(w))
		// The index builders below only read the AST and post-check tc tables
		// and write master fields prepare() never touches while they run; their
		// results are allocated in the helper's permanent thread arena. The
		// alias-suffix indexes must stay on the master: collect_types consults
		// them in declaration order.
		if os.getenv('V3_NO_PAR_TRANSFORM_PARAM_PREP') == '' {
			param_tc := t.tc.fork_for_parallel_transform(t.a)
			mut param_w := &Transformer{
				a:                            t.a
				tc:                           param_tc
				prefix_param_scan:            t.prefix_param_scan
				call_param_types_decl_cache:  map[int][]types.Type{}
				call_param_types_decl_misses: map[string]bool{}
				call_param_types_decl_index:  map[string]FnParamDeclRef{}
			}
			param_thread := spawn transform_param_prep_thread(voidptr(param_w))
			t.defer_pre_scan_indexes = true
			index_thread := spawn transform_pre_scan_index_thread(voidptr(t))
			t.prepare()
			_ = param_thread.wait()
			_ = index_thread.wait()
			t.call_param_types_decl_cache = param_w.call_param_types_decl_cache.move()
			t.call_param_types_decl_misses = param_w.call_param_types_decl_misses.move()
			t.call_param_types_decl_index = param_w.call_param_types_decl_index.move()
			t.call_param_types_index_ready = param_w.call_param_types_index_ready
			t.call_param_types_prepared = param_w.call_param_types_prepared
		} else {
			t.defer_pre_scan_indexes = true
			index_thread := spawn transform_pre_scan_index_thread(voidptr(t))
			t.prepare()
			_ = index_thread.wait()
		}
		_ = scan_thread.wait()
		t.defer_pre_scan_indexes = false
		for key, val in w.const_array_fixed_storage_cache {
			t.const_array_fixed_storage_cache[key.clone()] = val
		}
		t.const_array_fixed_storage_ready = true
		if w.worker_scope != unsafe { nil } {
			transform_worker_scope_free(w.worker_scope)
			w.worker_scope = unsafe { nil }
		}
	}
}
