module types

import os
import runtime
import time
import v3.flat
import v3.token
import v3.workers

const min_parallel_check_items = 256
const max_parallel_check_jobs = 26
const scoped_check_worker_batches = 8
// One chunk per worker makes the phase wall clock the single slowest chunk:
// span-based costs undercount construct-heavy bodies severalfold, so the other
// workers idle behind the outlier. Oversubscribed chunks let the pool queue
// rebalance dynamically for ~0.6 ms fork+merge overhead per extra chunk.
const check_chunk_oversubscribe = 4
// Extra share of the total work (in percent of an even bucket) pre-assigned to
// the master's bucket; see split_check_items.
const check_master_bias_pct = i64(0)

struct CheckWorkItem {
	fn_idx   int
	range_lo int // first node id owned by this fn (fn subtree = [range_lo, fn_idx])
	file     string
	module   string
	cost     int
	rank     i64
}

struct UnusedFnVarCandidate {
	name   string
	lhs_id flat.NodeId
	rhs_id flat.NodeId
}

$if !windows {
	struct CheckChunkArgs {
		worker        voidptr
		items_ptr     voidptr
		scope_enabled bool
	mut:
		scope voidptr
	}

	fn check_chunk_thread(arg voidptr) voidptr {
		mut a := unsafe { &CheckChunkArgs(arg) }
		a.scope = check_worker_scope_begin(a.scope_enabled)
		mut w := unsafe { &TypeChecker(a.worker) }
		items := unsafe { &[]CheckWorkItem(a.items_ptr) }
		if a.scope_enabled {
			w.check_scoped_batches(*items)
		} else {
			w.check_fn_items_serial(*items)
		}
		check_worker_scope_leave(a.scope)
		return unsafe { nil }
	}
}

// check_scoped_batches bounds each worker's temporary type-resolution state.
// The receiver is a result accumulator; every batch uses a fresh checker fork,
// then promotes only observable cache entries and diagnostics before its arena
// is released.
fn (mut tc TypeChecker) check_scoped_batches(items []CheckWorkItem) {
	if items.len == 0 {
		return
	}
	n_batches := if items.len < scoped_check_worker_batches {
		items.len
	} else {
		scoped_check_worker_batches
	}
	mut total_cost := i64(0)
	for item in items {
		total_cost += i64(item.cost) + 1
	}
	mut start := 0
	mut consumed_cost := i64(0)
	for batch_idx in 0 .. n_batches {
		mut end := start
		target_cost := total_cost * i64(batch_idx + 1) / i64(n_batches)
		for end < items.len
			&& (batch_idx == n_batches - 1 || consumed_cost < target_cost || end == start) {
			consumed_cost += i64(items[end].cost) + 1
			end++
		}
		scratch_scope := check_worker_scope_begin(true)
		mut batch := tc.fork_for_parallel_check()
		batch.check_fn_items_serial(items[start..end])
		check_worker_scope_leave(scratch_scope)
		tc.clone_parallel_worker_node_caches(items[start..end])
		tc.merge_parallel_check_worker_scoped(batch, true)
		check_worker_scope_free(scratch_scope)
		start = end
	}
}

fn check_worker_scope_begin(enabled bool) voidptr {
	$if prealloc {
		if enabled {
			return unsafe { prealloc_scope_begin() }
		}
	}
	return unsafe { nil }
}

fn check_worker_scope_leave(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_leave(scope) }
		}
	}
}

fn check_worker_scope_free(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_free_after(scope) }
		}
	}
}

// check_semantics_opt runs semantic checks, using worker threads for independent
// function bodies when requested and there is enough work.
pub fn (mut tc TypeChecker) check_semantics_opt(want_parallel bool) bool {
	error_count := tc.errors.len
	tc.check_for_in_const_conflicts_preflight()
	if tc.checker_fixture_mode && tc.errors.len > 0 {
		return false
	}
	if tc.errors.len == error_count {
		tc.check_comptime_for_source_types_preflight()
		struct_update_error_count := tc.errors.len
		tc.check_comptime_struct_updates_preflight()
		if tc.errors.len > struct_update_error_count {
			return false
		}
	}
	if !want_parallel {
		if tc.scope_parallel_check_workers {
			tc.check_semantics_scoped_serial()
		} else {
			tc.check_semantics()
		}
		return false
	}
	$if windows {
		tc.check_semantics()
		return false
	} $else {
		return tc.check_semantics_parallel()
	}
}

// check_semantics_scoped_serial keeps a no-parallel preallocated check bounded
// without starting helper threads. The serial semantic pass scopes each function
// independently when scope_parallel_check_workers is enabled.
fn (mut tc TypeChecker) check_semantics_scoped_serial() {
	tc.resolution_type_mode = false
	tc.install_type_cache_overlay()
	tc.defer_ierror_gating = tc.diagnostic_files.len > 0
	tc.selected_file_called_fns = map[string]bool{}
	tc.selected_file_worklist = []string{}
	tc.check_export_attrs()
	items := tc.collect_parallel_check_items()
	tc.check_top_level_declarations()
	final_file := tc.cur_file
	final_module := tc.cur_module
	tc.check_scoped_batches(items)
	tc.cur_file = final_file
	tc.cur_module = final_module
	if tc.defer_ierror_gating {
		if tc.pending_ierror_errors.len > 0 {
			tc.collect_selected_file_called_fns()
		}
		if tc.filter_pending_ierror_errors() {
			tc.sort_parallel_check_errors()
		}
		tc.defer_ierror_gating = false
	}
	tc.sort_parallel_check_errors()
	tc.restore_type_cache_base()
	tc.direct_parent_index_trusted = false
	tc.resolution_type_mode = true
}

// check_semantics_selected validates declarations and only the named function
// bodies. It is used by the function-level incremental compiler after it has
// proven that every top-level declaration is unchanged.
pub fn (mut tc TypeChecker) check_semantics_selected(selected map[string]bool) {
	tc.resolution_type_mode = false
	tc.check_export_attrs()
	items := tc.collect_parallel_check_items()
	tc.check_top_level_declarations()
	mut selected_items := []CheckWorkItem{cap: selected.len}
	for item in items {
		node := tc.a.nodes[item.fn_idx]
		qname := checker_qualified_fn_name(item.module, node.value)
		if selected[qname] || selected[node.value] {
			selected_items << item
		}
	}
	tc.check_fn_items_serial(selected_items)
	tc.direct_parent_index_trusted = false
	tc.resolution_type_mode = true
}

// check_semantics_reachable validates only selected function declarations and
// bodies, plus top-level statements in the selected input. It is intended for
// source shapes that have already proven they cannot declare any other items.
pub fn (mut tc TypeChecker) check_semantics_reachable(selected map[string]bool) {
	tc.resolution_type_mode = false
	tc.cur_module = ''
	tc.cur_file = ''
	mut items := []CheckWorkItem{cap: selected.len}
	mut prev_tl := -1
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
				if node.value in tc.diagnostic_files {
					tc.check_top_level_file_statements(node)
				}
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.fn_decl {
				qname := checker_qualified_fn_name(tc.cur_module, node.value)
				if selected[qname] || selected[node.value] {
					node_id := flat.NodeId(i)
					tc.check_fn_declaration_name(node_id, node)
					tc.check_main_fn_signature(node_id, node)
					tc.check_init_fn_signature(node_id, node)
					tc.check_str_method_signature(node_id, node)
					tc.check_free_method_signature(node_id, node)
					tc.check_sumtype_builtin_method_override(node_id, node)
					tc.check_test_fn_signature(node_id, node)
					tc.check_decl_type_strings(node_id, node)
					cost := i - prev_tl
					items << CheckWorkItem{
						fn_idx:   i
						range_lo: prev_tl + 1
						file:     tc.cur_file
						module:   tc.cur_module
						cost:     cost
						rank:     i64(cost) * 1_000_000_000 - i64(i)
					}
				}
			}
			.c_fn_decl {
				if selected[node.value] {
					tc.check_main_fn_signature(flat.NodeId(i), node)
				}
			}
			else {}
		}
		prev_tl = i
	}
	tc.check_fn_items_serial(items)
	tc.direct_parent_index_trusted = false
	tc.resolution_type_mode = true
}

fn (mut tc TypeChecker) check_semantics_parallel() bool {
	$if windows {
		tc.check_semantics()
		return false
	} $else {
		tc.resolution_type_mode = false
		// Freeze the warm post-collect type cache as the shared read-only base
		// for every worker thread and the master itself via a private overlay.
		tc.install_type_cache_overlay()
		// Invalid-IError-return diagnostics are gated to functions reachable
		// from the selected files. Most successful compiles never produce a
		// candidate, so defer the call-graph walk until after checking and only
		// run it when there is something to filter.
		tc.defer_ierror_gating = tc.diagnostic_files.len > 0
		tc.selected_file_called_fns = map[string]bool{}
		tc.selected_file_worklist = []string{}
		mut cksw := time.new_stopwatch()
		tc.check_export_attrs()
		tc.timing_profile('  [ttime]   ck export attrs  ${f64(cksw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		cksw.restart()
		items := tc.collect_parallel_check_items()
		tc.timing_profile('  [ttime]   ck collect items ${f64(cksw.elapsed().microseconds()) / 1000.0:7.2f} ms (items: ${items.len})')
		final_file := tc.cur_file
		final_module := tc.cur_module
		was_parallel := tc.run_parallel_check(items)
		tc.cur_file = final_file
		tc.cur_module = final_module
		if tc.defer_ierror_gating {
			if tc.pending_ierror_errors.len > 0 {
				tc.collect_selected_file_called_fns()
			}
			if tc.filter_pending_ierror_errors() {
				tc.sort_parallel_check_errors()
			}
			tc.defer_ierror_gating = false
		}
		tc.restore_type_cache_base()
		// Match the serial checker: only generated post-check type text may use the
		// cross-module generic-argument fallback.
		tc.direct_parent_index_trusted = false
		tc.resolution_type_mode = true
		return was_parallel
	}
}

fn (mut tc TypeChecker) filter_pending_ierror_errors() bool {
	mut added := false
	for p in tc.pending_ierror_errors {
		if p.fn_qname in tc.selected_file_called_fns {
			tc.errors << p.err
			added = true
		}
	}
	tc.pending_ierror_errors = []PendingIerrorError{}
	return added
}

fn (mut tc TypeChecker) collect_parallel_check_items() []CheckWorkItem {
	tc.cur_module = ''
	tc.cur_file = ''
	mut items := []CheckWorkItem{}
	// Fn subtrees are contiguous: the fn_decl at index i owns exactly the node
	// range (previous top-level node, i], so the span doubles as the cost
	// estimate (replacing a full subtree walk per fn). The declaration-level
	// checks the old walk ran inline live in check_top_level_declarations,
	// which the parallel flow runs on the master while workers check bodies.
	mut prev_tl := -1
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.fn_decl {
				cost := i - prev_tl
				items << CheckWorkItem{
					fn_idx:   i
					range_lo: prev_tl + 1
					file:     tc.cur_file
					module:   tc.cur_module
					cost:     cost
					rank:     i64(cost) * 1_000_000_000 - i64(i)
				}
			}
			else {}
		}
		prev_tl = i
	}
	return items
}

// check_top_level_declarations runs every declaration-level check (type
// strings, signatures, const/enum field values) in the original interleaved
// declaration order. Serial flows call it directly; the parallel flow splits
// it via check_top_level_declarations_filtered.
fn (mut tc TypeChecker) check_top_level_declarations() {
	tc.check_top_level_declarations_filtered(true, true)
}

// check_top_level_declaration_values runs only the initializer-value checks
// (top-level statements, struct field defaults, enum values, const values).
// These can mutate compilation-wide state — check_const_field_values poisons
// tc.const_types for cyclic constants — and their results must be visible to
// every body worker, so the parallel flow runs them before submitting chunks.
fn (mut tc TypeChecker) check_top_level_declaration_values() {
	tc.check_top_level_declarations_filtered(true, false)
}

// check_top_level_declaration_signatures runs the declaration checks that only
// read the frozen collect tables and record diagnostics (type strings and
// signature shapes; alias-cycle and C-redeclaration diagnostics stay in the
// collection phase, where direct collect() callers expect them). The parallel
// flow runs these on the master thread while the pool workers check bodies.
fn (mut tc TypeChecker) check_top_level_declaration_signatures() {
	tc.check_top_level_declarations_filtered(false, true)
}

fn (mut tc TypeChecker) check_top_level_declarations_filtered(do_values bool, do_signatures bool) {
	tc.cur_module = ''
	tc.cur_file = ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
				if do_values {
					tc.check_top_level_file_statements(node)
				}
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.struct_decl {
				node_id := flat.NodeId(i)
				if do_signatures {
					if comma_attr_text_has(node.typ, 'typedef') && !node.value.starts_with('C.') {
						tc.record_error_at(.assignment_mismatch,
							'`typedef` attribute can only be used with C structs', node_id, tc.declaration_keyword_name_pos(node_id,
							'struct'))
					}
					tc.check_decl_type_strings(flat.NodeId(i), node)
				}
				if do_values {
					tc.check_struct_field_defaults(node_id, node)
				}
			}
			.type_decl, .interface_decl {
				if !do_signatures {
					continue
				}
				node_id := flat.NodeId(i)
				if node.kind == .type_decl && tc.type_declaration_exists_before(node_id, node.value) {
					kind := if node.children_count > 0 || split_sum_variant_texts(node.typ).len > 1 {
						'sum type'
					} else {
						'alias'
					}
					tc.record_error_at(.duplicate_decl,
						'cannot register ${kind} `${node.value}`, another type with this name exists',
						node_id, tc.declaration_keyword_name_pos(node_id, 'type'))
				}
				tc.check_decl_type_strings(flat.NodeId(i), node)
			}
			.enum_decl {
				if do_values {
					tc.check_enum_field_values(flat.NodeId(i), node)
				}
			}
			.const_decl {
				if do_values {
					tc.check_const_field_values(node)
				}
			}
			.fn_decl {
				if !do_signatures {
					continue
				}
				tc.check_fn_declaration_name(flat.NodeId(i), node)
				tc.check_main_fn_signature(flat.NodeId(i), node)
				tc.check_init_fn_signature(flat.NodeId(i), node)
				tc.check_str_method_signature(flat.NodeId(i), node)
				tc.check_free_method_signature(flat.NodeId(i), node)
				tc.check_sumtype_builtin_method_override(flat.NodeId(i), node)
				tc.check_test_fn_signature(flat.NodeId(i), node)
				tc.check_decl_type_strings(flat.NodeId(i), node)
			}
			.c_fn_decl {
				if !do_signatures {
					continue
				}
				tc.check_main_fn_signature(flat.NodeId(i), node)
				if tc.reject_unsupported_generics {
					tc.check_decl_type_strings(flat.NodeId(i), node)
				}
			}
			else {}
		}
	}
	if do_signatures {
		tc.check_test_file_has_test_fn()
	}
}

fn check_top_level_decl_signatures_thread(arg voidptr) voidptr {
	mut tc := unsafe { &TypeChecker(arg) }
	tc.check_top_level_declaration_signatures()
	return unsafe { nil }
}

fn (mut tc TypeChecker) run_parallel_check(items []CheckWorkItem) bool {
	$if windows {
		tc.check_top_level_declarations()
		tc.check_fn_items_serial(items)
		return false
	} $else {
		mut ast := unsafe { tc.a }
		if isnil(ast.worker_pool) {
			ast.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		n_jobs := check_job_count(ast.worker_pool.size() + 1, items.len)
		if items.len < min_parallel_check_items || n_jobs <= 1 {
			tc.check_top_level_declarations()
			tc.check_fn_items_serial(items)
			return false
		}
		// Initializer-value checks can mutate compilation-wide state that the
		// body workers read (for example the const-cycle poisoning of
		// tc.const_types), so they must complete before any chunk is
		// submitted; only the read-only signature checks overlap the pool.
		tc.check_top_level_declaration_values()
		mut chunk_target := n_jobs
		if tc.scope_parallel_check_workers {
			chunk_target = n_jobs * check_chunk_oversubscribe
			if chunk_target > items.len {
				chunk_target = items.len
			}
		}
		mut chunks := split_check_items(items, chunk_target)
		chunk_count := chunks.len
		thread_count := chunk_count - 1
		setup_scope := check_worker_scope_begin(tc.scope_parallel_check_workers)
		worker_count := if tc.scope_parallel_check_workers { chunk_count } else { thread_count }
		rpsw := time.new_stopwatch()
		mut checker_workers := []voidptr{cap: worker_count}
		for _ in 0 .. worker_count {
			w := tc.fork_for_parallel_check()
			checker_workers << voidptr(w)
		}
		tc.timing_profile('  [ttime]   ck forks         ${f64(rpsw.elapsed().microseconds()) / 1000.0:7.2f} ms (workers: ${worker_count})')
		mut args := []CheckChunkArgs{cap: chunk_count}
		for ci in 0 .. chunk_count {
			mut worker := voidptr(tc)
			if tc.scope_parallel_check_workers {
				worker = checker_workers[ci]
			} else if ci > 0 {
				worker = checker_workers[ci - 1]
			}
			args << CheckChunkArgs{
				worker:        worker
				items_ptr:     unsafe { voidptr(&chunks[ci]) }
				scope_enabled: tc.scope_parallel_check_workers
			}
		}
		// The master checks its own chunk under the same range discipline as the
		// workers: in-range cache writes go straight into the shared arrays (the
		// master owns those slots), out-of-range ones into its sparse maps, which
		// are replayed first after join so that worker merges overwrite them in
		// the same order the old serial flow did. The read-only signature checks
		// run as a final synchronous task, so the master performs them while the
		// pool workers are still checking bodies; its sparse mode keeps their
		// cache writes out of the worker-owned shared array ranges. The mutating
		// value checks already ran before any chunk was created.
		tc.parallel_check_sparse = true
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: chunk_count + 1}
		for ci in 0 .. chunk_count {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        check_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'checker:all' || fail == 'checker:${helper_idx}'
			}
		}
		tasks << workers.Task{
			run:        check_top_level_decl_signatures_thread
			arg:        voidptr(tc)
			force_sync: true
		}
		check_worker_scope_leave(setup_scope)
		rpsw2 := time.new_stopwatch()
		any_started := ast.worker_pool.run(tasks)
		tc.timing_profile('  [ttime]   ck pool.run      ${f64(rpsw2.elapsed().microseconds()) / 1000.0:7.2f} ms (chunks: ${chunk_count})')
		tc.merge_own_sparse_caches()
		tc.parallel_check_sparse = false
		merge_start := if tc.scope_parallel_check_workers { 0 } else { 1 }
		// Promote scope-resident cache payloads across the pool while every
		// worker scope is still alive; only unknown-type interning stays serial.
		par_clone := tc.scope_parallel_check_workers && par_check_clone_enabled()
		mut clone_args := []CheckCloneChunkArgs{cap: chunk_count}
		mut mg_clone_ns := u64(0)
		mut mg_merge_ns := u64(0)
		if par_clone {
			mg_t0 := time.sys_mono_now()
			for ci in 0 .. chunk_count {
				clone_args << CheckCloneChunkArgs{
					tc:        voidptr(tc)
					items_ptr: unsafe { voidptr(&chunks[ci]) }
					miss:      []int{cap: 1024}
				}
			}
			mut ctasks := []workers.Task{cap: chunk_count}
			for ci in 0 .. chunk_count {
				ctasks << workers.Task{
					run:        check_clone_chunk_thread
					arg:        unsafe { voidptr(&clone_args[ci]) }
					force_sync: ci == 0
				}
			}
			ast.worker_pool.run(ctasks)
			mg_clone_ns += time.sys_mono_now() - mg_t0
		}
		for ci in merge_start .. chunk_count {
			worker_idx := if tc.scope_parallel_check_workers { ci } else { ci - 1 }
			mut w := unsafe { &TypeChecker(checker_workers[worker_idx]) }
			scoped := args[ci].scope != unsafe { nil }
			if scoped {
				mg_t0 := time.sys_mono_now()
				if par_clone {
					tc.intern_expr_type_misses(clone_args[ci].miss)
				} else {
					tc.clone_parallel_worker_node_caches(chunks[ci])
				}
				mg_clone_ns += time.sys_mono_now() - mg_t0
			}
			mg_t1 := time.sys_mono_now()
			tc.merge_parallel_check_worker_scoped(w, scoped)
			mg_merge_ns += time.sys_mono_now() - mg_t1
			if scoped {
				check_worker_scope_free(args[ci].scope)
			} else {
				w.free_parallel_check_worker_cache()
			}
		}
		mg_clone_ms := f64(mg_clone_ns) / 1e6
		mg_merge_ms := f64(mg_merge_ns) / 1e6
		tc.timing_profile('  [ttime]     ck mg clone    ${mg_clone_ms:7.2f} ms, merge ${mg_merge_ms:.2f} ms')
		tc.timing_profile('  [ttime]   ck merge         ${f64(rpsw2.elapsed().microseconds()) / 1000.0:7.2f} ms (cumulative)')
		check_worker_scope_free(setup_scope)
		tc.sort_parallel_check_errors()
		return any_started
	}
}

fn (mut tc TypeChecker) sort_parallel_check_errors() {
	tc.errors.sort_with_compare(compare_type_errors)
	tc.notices.sort_with_compare(compare_type_notices)
	if tc.errors.len < 2 {
		return
	}
	mut deduped := []TypeError{cap: tc.errors.len}
	for err in tc.errors {
		if deduped.len > 0 && type_errors_equal(deduped[deduped.len - 1], err) {
			continue
		}
		deduped << err
	}
	tc.errors = deduped
}

fn compare_type_notices(a &TypeError, b &TypeError) int {
	a_is_unsafe_call := a.msg.contains('must be called from an `unsafe` block')
	b_is_unsafe_call := b.msg.contains('must be called from an `unsafe` block')
	if a_is_unsafe_call != b_is_unsafe_call {
		return if a_is_unsafe_call { -1 } else { 1 }
	}
	a_is_reference_assignment := a.msg.starts_with('cannot assign a reference to a value')
	b_is_reference_assignment := b.msg.starts_with('cannot assign a reference to a value')
	if a_is_reference_assignment != b_is_reference_assignment {
		return if a_is_reference_assignment { -1 } else { 1 }
	}
	a_is_unused := a.msg.starts_with('unused parameter:')
	b_is_unused := b.msg.starts_with('unused parameter:')
	if a_is_unused != b_is_unused {
		return if a_is_unused { 1 } else { -1 }
	}
	return compare_type_errors(a, b)
}

fn compare_type_errors(a &TypeError, b &TypeError) int {
	a_is_c_string_buffer_conversion := a.msg.starts_with('to convert a C string buffer pointer')
	b_is_c_string_buffer_conversion := b.msg.starts_with('to convert a C string buffer pointer')
	a_is_pointer_string_cast := a.msg.starts_with('cannot cast pointer type ')
		&& a.msg.contains(' to string')
	b_is_pointer_string_cast := b.msg.starts_with('cannot cast pointer type ')
		&& b.msg.contains(' to string')
	if a_is_c_string_buffer_conversion && b_is_pointer_string_cast {
		return -1
	}
	if b_is_c_string_buffer_conversion && a_is_pointer_string_cast {
		return 1
	}
	a_is_option_result_split := a.msg.starts_with('Option and Result types have been split')
	b_is_option_result_split := b.msg.starts_with('Option and Result types have been split')
	a_is_none_result_mismatch := a.msg.starts_with('cannot use `none` as Result type')
	b_is_none_result_mismatch := b.msg.starts_with('cannot use `none` as Result type')
	if a_is_option_result_split && b_is_none_result_mismatch {
		return -1
	}
	if b_is_option_result_split && a_is_none_result_mismatch {
		return 1
	}
	a_is_option_array_push := a.msg == 'cannot push to Option array that was not unwrapped first'
	b_is_option_array_push := b.msg == 'cannot push to Option array that was not unwrapped first'
	a_is_option_array_unwrap := a.msg.contains('cannot be used as `[]')
		&& a.msg.ends_with('unwrap the option first')
	b_is_option_array_unwrap := b.msg.contains('cannot be used as `[]')
		&& b.msg.ends_with('unwrap the option first')
	if (a_is_option_array_push || a_is_option_array_unwrap)
		&& (b_is_option_array_push || b_is_option_array_unwrap) {
		if a.pos.id != b.pos.id {
			return a.pos.id - b.pos.id
		}
		if a.pos.offset != b.pos.offset {
			return a.pos.offset - b.pos.offset
		}
		if a_is_option_array_push != b_is_option_array_push {
			return if a_is_option_array_push { -1 } else { 1 }
		}
	}
	a_is_bare_generic_fntype_decl := a.msg.starts_with('generic function `')
		&& a.msg.contains(' in fn declaration must specify the generic type names')
	b_is_bare_generic_fntype_decl := b.msg.starts_with('generic function `')
		&& b.msg.contains(' in fn declaration must specify the generic type names')
	if a_is_bare_generic_fntype_decl != b_is_bare_generic_fntype_decl {
		return if a_is_bare_generic_fntype_decl { 1 } else { -1 }
	}
	if a.node != b.node {
		return int(a.node) - int(b.node)
	}
	if a.kind != b.kind {
		return int(a.kind) - int(b.kind)
	}
	if a.msg < b.msg {
		return -1
	}
	if a.msg > b.msg {
		return 1
	}
	return 0
}

fn type_errors_equal(a TypeError, b TypeError) bool {
	return a.node == b.node && a.kind == b.kind && a.msg == b.msg
}

fn check_job_count(n_runtime_jobs int, n_items int) int {
	if n_runtime_jobs <= 0 || n_items <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_parallel_check_jobs {
		n = max_parallel_check_jobs
	}
	if n > n_items {
		n = n_items
	}
	return n
}

fn split_check_items(items []CheckWorkItem, n int) [][]CheckWorkItem {
	mut buckets := [][]CheckWorkItem{len: n, init: []CheckWorkItem{}}
	mut loads := []i64{len: n}
	if n > 1 && check_master_bias_pct != 0 {
		// Historical bias: bucket 0 once finished early on this split. Measured
		// 2026-07-25 the master chunk was instead the pool tail by ~30% (its
		// span cost undercounts the dense builtin bodies), so the bias is off;
		// the knob stays for machines where the old premise holds.
		mut total := i64(0)
		for it in items {
			total += i64(it.cost) + 1
		}
		loads[0] = -total * check_master_bias_pct / i64(100 * n)
	}
	mut sorted := items.clone()
	sorted.sort(a.rank > b.rank)
	for it in sorted {
		mut best := 0
		for b in 1 .. n {
			if loads[b] < loads[best] {
				best = b
			}
		}
		buckets[best] << it
		loads[best] += i64(it.cost) + 1
	}
	for mut bucket in buckets {
		bucket.sort(a.fn_idx < b.fn_idx)
	}
	return buckets
}

// merge_own_sparse_caches replays the master's out-of-range cache writes
// (parked in its sparse maps while it checked its own chunk under the range
// discipline) into the shared node-indexed arrays, restoring the state the old
// serial flow produced with direct writes.
fn (mut tc TypeChecker) merge_own_sparse_caches() {
	for idx, name in tc.sparse_resolved_call_names {
		tc.resolved_call_names[idx] = name
		tc.resolved_call_set[idx] = true
	}
	for idx, name in tc.sparse_resolved_fn_values {
		tc.resolved_fn_value_names[idx] = name
		tc.resolved_fn_value_set[idx] = true
	}
	for idx, _ in tc.sparse_statement_nodes {
		tc.statement_nodes[idx] = true
	}
	for idx, typ in tc.sparse_expr_type_values {
		tc.expr_type_values[idx] = typ
		tc.expr_type_set[idx] = true
	}
	tc.sparse_resolved_call_names.clear()
	tc.sparse_resolved_fn_values.clear()
	tc.sparse_statement_nodes.clear()
	tc.sparse_expr_type_values.clear()
	tc.sparse_checking_nodes.clear()
}

fn (mut tc TypeChecker) check_fn_items_serial(items []CheckWorkItem) {
	if isnil(tc.body_resolve_memo) {
		tc.body_resolve_memo = &BodyResolveMemo{}
	}
	mut memo := tc.body_resolve_memo
	for it in items {
		node := tc.a.nodes[it.fn_idx]
		tc.check_range_lo = it.range_lo
		tc.check_range_hi = it.fn_idx
		memo.begin(it.range_lo, it.fn_idx)
		tc.check_fn_decl_semantics(it.fn_idx, node, it.file, it.module)
	}
	memo.active = false
	tc.check_range_lo = -1
	tc.check_range_hi = -1
}

// check_concrete_fn_semantics validates a concrete generic function clone before
// the transformer lowers its body. The clone retains source positions, so errors use
// the normal checker renderer instead of positionless transform diagnostics.
pub fn (mut tc TypeChecker) check_concrete_fn_semantics(fn_idx int, file string, module_name string) {
	tc.extend_node_caches(tc.a.nodes.len)
	if fn_idx < 0 || fn_idx >= tc.a.nodes.len {
		return
	}
	node := tc.a.nodes[fn_idx]
	if node.kind != .fn_decl {
		return
	}
	tc.selected_file_called_fns[checker_qualified_fn_name(module_name, node.value)] = true
	tc.check_fn_decl_semantics(fn_idx, node, file, module_name)
}

fn (mut tc TypeChecker) check_fn_decl_semantics(fn_idx int, node flat.Node, file string, module_name string) {
	saved_fn_context := tc.fn_context
	tc.fn_context = new_function_check_context()
	inferred_generic_params := tc.infer_decl_generic_param_names(node)
	is_concrete_generic_receiver := node.value.contains('.')
		&& node.value.all_before_last('.').contains('[') && inferred_generic_params.len == 0
	is_specialized := tc.a.specialized_fn_nodes[fn_idx] || is_concrete_generic_receiver
	tc.fn_context.generic_params = if is_specialized {
		[]string{}
	} else {
		inferred_generic_params
	}
	tc.cur_file = file
	tc.cur_module = module_name
	if module_name in ['', 'main'] && !node.value.contains('.') {
		if visibility := tc.declaration_visibility['builtin.${node.value}'] {
			if visibility.is_pub {
				tc.record_error_at(.duplicate_decl,
					'cannot redefine builtin public function `${node.value}`', flat.NodeId(fn_idx),
					tc.fn_declaration_diagnostic_pos(node))
				tc.fn_context = saved_fn_context
				return
			}
		}
	}
	tc.cur_scope = tc.file_scope
	checked_return_type := if node.typ.ends_with('?') && !node.typ.starts_with('?') {
		node.typ.trim_right('?')
	} else {
		node.typ
	}
	tc.cur_fn_ret_type = tc.parse_type(checked_return_type)
	tc.fn_context.return_type = tc.cur_fn_ret_type
	tc.fn_context.node_id = fn_idx
	tc.fn_context.concrete_generic_receiver_specialization =
		fn_value_is_concrete_generic_receiver_specialization(node.value)
	tc.cur_fn_node_id = fn_idx
	tc.method_value_locals = map[string]bool{}
	tc.method_value_local_depth = map[string]int{}
	tc.capturing_fn_literal_locals = map[string]bool{}
	tc.capturing_fn_literal_local_depth = map[string]int{}
	tc.capturing_fn_literal_return_unsupported = map[string]bool{}
	tc.check_fn_receiver_and_operator_return(node, flat.NodeId(fn_idx))
	$if ownership ? {
		tc.ownership_begin_fn(node)
	}
	tc.push_scope()
	if module_name != 'builtin' && node.value.ends_with('.map') && node.children_count > 0 {
		receiver := tc.a.child_node(&node, 0)
		if receiver.kind == .param && receiver.typ.trim_left('&').starts_with('[]') {
			tc.record_error_at(.call_arg_mismatch, 'method overrides built-in array method',
				flat.NodeId(fn_idx), tc.fn_declaration_diagnostic_pos(node))
		}
	}
	mut parameter_names := map[string]bool{}
	mut duplicate_parameter_ids := map[int]bool{}
	for pi in 0 .. node.children_count {
		param_id := tc.a.child(&node, pi)
		param := tc.a.node(param_id)
		if param.kind == .param {
			if param.value.len > 0 && param.value != '_' {
				if parameter_names[param.value] {
					tc.record_error_at(.duplicate_decl,
						'redefinition of parameter `${param.value}`', param_id,
						tc.node_value_diagnostic_pos(param_id))
					duplicate_parameter_ids[int(param_id)] = true
				} else {
					parameter_names[param.value] = true
				}
			}
			if param.is_mut {
				implicit_mut_reference := param.op != .amp && param.typ.starts_with('&')
				diagnostic_type_text := if implicit_mut_reference {
					param.typ[1..]
				} else {
					param.typ
				}
				raw_param_type := tc.parse_scope_param_type(diagnostic_type_text)
				if tc.is_params_struct_type(raw_param_type) {
					tc.record_error_at(.call_arg_mismatch,
						'declaring a mutable parameter that accepts a struct with the `@[params]` attribute is not allowed',
						param_id, tc.type_diagnostic_pos(param_id, diagnostic_type_text))
				}
				param_type := unalias_type(raw_param_type)
				if !is_specialized && param_type !is Array && param_type !is ArrayFixed
					&& param_type !is Interface && param_type !is Map && param_type !is Pointer
					&& param_type !is Struct && param_type !is SumType && param_type !is Unknown {
					if !(param.op == .dot && param_type is OptionType) {
						type_name := param_type.name()
						tc.record_error_at(.call_arg_mismatch,
							'mutable arguments are only allowed for arrays, interfaces, maps, pointers, structs or their aliases\nreturn values instead: `fn foo(mut n ${type_name}) {` => `fn foo(n ${type_name}) ${type_name} {`',
							param_id, tc.type_diagnostic_pos(param_id, diagnostic_type_text))
					}
				}
			}
			tc.check_reserved_parameter_name(param_id)
			if param.op == .dot {
				tc.check_import_symbol_conflict_at(param_id, param.value, tc.fn_receiver_param_diagnostic_pos(node,
					param.value))
			} else {
				tc.check_import_symbol_conflict(param_id, param.value)
			}
			tc.check_module_name_conflict(param_id, param.value)
		}
	}
	if !node.value.contains('.') && tc.has_active_import(node.value) {
		tc.record_error_at(.duplicate_decl, 'duplicate of an import symbol `${node.value}`',
			flat.NodeId(fn_idx), tc.fn_declaration_diagnostic_pos(node))
	}
	for pi in 0 .. node.children_count {
		param_id := tc.a.child(&node, pi)
		if duplicate_parameter_ids[int(param_id)] {
			continue
		}
		p := tc.a.node(param_id)
		tc.insert_fn_param_binding(p)
	}
	tc.insert_implicit_veb_ctx(node)
	tc.check_veb_app_method_params(flat.NodeId(fn_idx), node)
	// Open generic declarations are checked when they are instantiated.  Walking every
	// template in a selected module diagnoses names that only exist after comptime
	// expansion (and even dead generic helpers), unlike the reference compiler.
	generic_params := if is_specialized {
		map[string]bool{}
	} else {
		tc.infer_decl_generic_params(node)
	}
	qname := checker_qualified_fn_name(module_name, node.value)
	signature_has_bare_generic_type := tc.fn_decl_has_bare_generic_signature_type(node)
	should_check_generic_body := generic_params.len == 0 || qname in tc.selected_file_called_fns
	if should_check_generic_body && !signature_has_bare_generic_type {
		tc.check_fn_body(node)
		tc.check_recursive_str_calls(flat.NodeId(fn_idx), node)
	} else if generic_params.len > 0 && node.value.contains('.')
		&& tc.should_diagnose(flat.NodeId(fn_idx)) {
		tc.check_deferred_generic_receiver_comparisons(node)
	}
	tc.check_noreturn_fn_semantics(flat.NodeId(fn_idx), node, qname)
	tc.check_unreachable_after_noreturn_call(node)
	if !is_specialized {
		if tc.should_diagnose(flat.NodeId(fn_idx)) {
			tc.record_unused_fn_vars(node)
			tc.record_unused_fn_params(node)
			tc.record_unused_fn_labels(node)
		}
		tc.check_fn_bare_generic_fntype_params(node)
	}
	tc.fn_context.node_id = -1
	is_disabled_stub := node.value in tc.a.disabled_fns
	// A terminal propagation whose payload still contains a generic placeholder
	// and return control flow guarded by a generic `$if` are lowered against the
	// concrete specialization. Keep those narrow deferrals without suppressing
	// ordinary generic fallthrough.
	has_deferred_generic_return := generic_params.len > 0
		&& tc.fn_has_deferred_generic_return(node, generic_params)
	if tc.fn_context.return_type !is Unknown
		&& !type_allows_implicit_return(tc.fn_context.return_type)
		&& !tc.fn_body_definitely_returns(node) && !is_disabled_stub && !has_deferred_generic_return
		&& should_check_generic_body && !signature_has_bare_generic_type
		&& tc.should_diagnose(flat.NodeId(fn_idx)) {
		message := 'missing return at end of function `${node.value.all_after_last('.')}`'
		tc.record_error_at(.return_mismatch, message, flat.NodeId(fn_idx),
			tc.fn_declaration_diagnostic_pos(node))
	}
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_fn()
	}
	tc.fn_context = saved_fn_context
}

fn (mut tc TypeChecker) check_fn_receiver_and_operator_return(node flat.Node, id flat.NodeId) {
	tc.check_fn_receiver_syntax(id, node)
	if node.children_count > 0 {
		receiver_id := tc.a.child(&node, 0)
		receiver := tc.a.node(receiver_id)
		if receiver.kind == .param && receiver.op == .dot
			&& unalias_type(tc.parse_type(receiver.typ)) is MultiReturn {
			tc.record_error_at(.call_arg_mismatch, 'cannot define method on multi-value',
				receiver_id, tc.type_diagnostic_pos(receiver_id, receiver.typ))
		}
	}
	raw_return_type := node.typ.trim_space()
	if raw_return_type.starts_with('!?') || raw_return_type.starts_with('?!') {
		tc.record_error_at(.return_mismatch, 'the type must be Option or Result', id,
			tc.nested_option_result_marker_pos(node))
	}
	if raw_return_type == '?void' {
		tc.record_error_at(.return_mismatch, 'use `?` instead of `?void`', id,
			tc.option_void_payload_diagnostic_pos(node))
	}
	if raw_return_type.ends_with('?') && !raw_return_type.starts_with('?') {
		tc.record_error_at(.return_mismatch,
			'wrong syntax, it must be ?${raw_return_type.trim_right('?')}, not ${raw_return_type}',
			id, tc.suffix_option_return_type_diagnostic_pos(node))
	}
	signature_return_type := unalias_type(tc.parse_type(node.typ))
	if signature_return_type is MultiReturn {
		for typ in signature_return_type.types {
			if is_ierror_type(unalias_type(typ)) {
				tc.record_error_at(.return_mismatch,
					'type `IError` cannot be used in multi-return, return an Option instead', id,
					tc.fn_return_type_diagnostic_pos(node))
				break
			}
		}
	}
	if node.children_count > 0 && node.value.contains('.') {
		receiver := tc.a.child_node(&node, 0)
		receiver_name := node.value.all_before_last('.').all_after_last('.')
		if receiver.kind == .param {
			receiver_type := unalias_type(tc.parse_type(receiver.typ))
			if receiver_type is Interface
				&& node.value.all_after_last('.') in tc.interface_abstract_method_names(receiver_type.name) {
				tc.record_error_at(.duplicate_decl,
					'interface `${receiver_type.name}` cannot implement its own interface method `${node.value.all_after_last('.')}`',
					id, tc.fn_declaration_diagnostic_pos(node))
			}
			if receiver_type is OptionType || receiver.typ.contains('?')
				|| receiver_name.starts_with('?') {
				tc.record_error_at(.call_arg_mismatch, 'option types cannot have methods', id, tc.fn_option_receiver_diagnostic_pos(node,
					receiver.value))
			}
		}
	}
	if raw_return_type.starts_with('!') {
		alias_name := raw_return_type[1..]
		alias_target := tc.type_aliases[alias_name] or {
			tc.type_aliases[tc.qualify_name(alias_name)] or { '' }
		}
		if alias_target.starts_with('?') {
			tc.record_error_at(.return_mismatch,
				'the fn returns type `${raw_return_type}`, but type `${alias_name}` is an Option alias, you can not mix them',
				id, tc.fn_return_type_diagnostic_pos(node))
		}
	}
	operator := node.value.all_after_last('.')
	if !node.value.contains('.')
		|| operator !in ['+', '-', '*', '/', '%', '==', '!=', '<', '<=', '>', '>=', '<<', '>>', '&', '|', '^'] {
		return
	}
	mut param_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind == .param {
			param_ids << child_id
		}
	}
	if param_ids.len != 2 {
		tc.record_error_at(.call_arg_mismatch, 'operator methods should have exactly 1 argument',
			id, tc.fn_declaration_diagnostic_pos(node))
	}
	mut operator_params_match := param_ids.len == 2
	if param_ids.len > 0 {
		receiver_id := param_ids[0]
		receiver := tc.a.node(receiver_id)
		if receiver.is_mut {
			tc.record_error_at(.call_arg_mismatch,
				'receiver cannot be `mut` for operator overloading', receiver_id,
				tc.operator_receiver_without_mut_pos(node))
		}
		if param_ids.len > 1 {
			param_id := param_ids[1]
			param := tc.a.node(param_id)
			if param.is_mut {
				tc.record_error_at(.call_arg_mismatch,
					'argument cannot be `mut` for operator overloading', id,
					tc.fn_declaration_diagnostic_pos(node))
			}
			raw_receiver_type := tc.parse_type(receiver.typ)
			raw_param_type := tc.parse_type(param.typ)
			receiver_type := unwrap_pointer(raw_receiver_type)
			param_type := unwrap_pointer(raw_param_type)
			if !receiver.is_mut && !param.is_mut && receiver_type.name() == param_type.name()
				&& raw_receiver_type.name() != raw_param_type.name() {
				operator_params_match = false
				tc.record_error_at(.call_arg_mismatch,
					'the receiver type `${receiver.typ}` should be the same type as the operand `${param.typ}`',
					id, tc.fn_declaration_diagnostic_pos(node))
			}
			if receiver_type.name() != param_type.name() {
				operator_params_match = false
				tc.record_error_at(.call_arg_mismatch,
					'expected `${receiver_type.name()}` not `${param_type.name()}` - both operands must be the same type for operator overloading',
					param_id, tc.type_diagnostic_pos(param_id, param_type.name()))
			}
		}
	}
	parsed_return_type := tc.parse_type(node.typ)
	return_type := unalias_type(parsed_return_type)
	if return_type is OptionType || return_type is ResultType {
		tc.record_error_at(.return_mismatch, 'return type cannot be Option or Result', id,
			tc.fn_return_type_diagnostic_pos(node))
	}
	if node.children_count > 0 && operator in ['+', '-', '*', '/', '%', '<<', '>>', '&', '|', '^'] {
		receiver := tc.a.child_node(&node, 0)
		receiver_type := tc.parse_type(receiver.typ)
		if receiver_type is Alias && (infix_power_type_is_numeric(receiver_type.base_type)
			|| unalias_type(receiver_type.base_type) is String)
			&& parsed_return_type.name() != receiver_type.name && operator_params_match {
			tc.record_error_at(.return_mismatch,
				'operator `${operator}` methods on primitive aliases should return `${receiver_type.name}`',
				id, tc.fn_return_type_diagnostic_pos(node))
		}
	}
}

fn (tc &TypeChecker) operator_receiver_without_mut_pos(node flat.Node) token.Pos {
	text, pos := tc.fn_receiver_source_text_pos(node)
	if text.starts_with('(mut ') && pos.end - pos.offset > 6 {
		return token.new_span(pos.id, pos.offset + 5, pos.end - 1)
	}
	return pos
}

fn (mut tc TypeChecker) record_unused_fn_vars(node flat.Node) {
	if tc.node_is_from_translated_file(node) {
		return
	}
	for diagnostic in tc.errors {
		if diagnostic.msg == 'expecting `:=` (e.g. `mut x :=`)' {
			return
		}
	}
	if tc.checker_fixture_mode {
		tc.record_lambda_capture_errors(node)
	}
	mut candidates := []UnusedFnVarCandidate{}
	mut candidate_names := map[string]bool{}
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		current := tc.a.node(id)
		if current.kind == .decl_assign {
			lhs_ids := tc.multi_assign_lhs_ids(current)
			rhs_count := tc.multi_assign_rhs_count(current)
			for lhs_index, lhs_id in lhs_ids {
				lhs := tc.a.node(lhs_id)
				if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_'
					|| lhs.value.starts_with('_') {
					continue
				}
				rhs_id := if rhs_count == 1 {
					tc.multi_assign_rhs_id(current, 0)
				} else if lhs_index < rhs_count {
					tc.multi_assign_rhs_id(current, lhs_index)
				} else {
					flat.empty_node
				}
				candidates << UnusedFnVarCandidate{
					name:   lhs.value
					lhs_id: lhs_id
					rhs_id: rhs_id
				}
				candidate_names[lhs.value] = true
			}
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	if candidates.len == 0 {
		return
	}
	used_names := tc.fn_body_read_names(node, candidate_names)
	for candidate in candidates {
		if used_names[candidate.name] {
			continue
		}
		if tc.expr_subtree_has_error_except(candidate.rhs_id, .if_branch_mismatch)
			&& !tc.expr_subtree_allows_unused_warning(candidate.rhs_id) {
			continue
		}
		tc.record_warning_at(.unknown_ident, 'unused variable: `${candidate.name}`',
			candidate.lhs_id, tc.node_value_diagnostic_pos(candidate.lhs_id))
	}
}

fn (mut tc TypeChecker) record_lambda_capture_errors(fn_node flat.Node) {
	mut declarations := map[string]flat.NodeId{}
	mut stack := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(&fn_node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		node := tc.a.node(id)
		if node.kind == .lambda_expr {
			continue
		}
		if node.kind == .decl_assign {
			for lhs_id in tc.multi_assign_lhs_ids(node) {
				lhs := tc.a.node(lhs_id)
				if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_'
					&& lhs.value !in declarations {
					declarations[lhs.value] = lhs_id
				}
			}
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	stack.clear()
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(&fn_node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		node := tc.a.node(id)
		if node.kind == .lambda_expr && node.children_count > 0 {
			mut params := map[string]bool{}
			for i in 0 .. node.children_count - 1 {
				param := tc.a.child_node(node, i)
				if param.kind == .ident {
					params[param.value] = true
				}
			}
			body_id := tc.a.child(node, node.children_count - 1)
			if capture_id := tc.lambda_capture_ident(body_id, params, declarations) {
				capture := tc.a.node(capture_id)
				undefined_message := 'undefined variable `${capture.value}`'
				capture_pos := tc.node_value_diagnostic_pos(capture_id)
				if !tc.errors.any(it.msg == undefined_message && it.pos.id == capture_pos.id
					&& it.pos.offset == capture_pos.offset && it.pos.end == capture_pos.end) {
					tc.errors << tc.make_type_error_at(.unknown_ident, undefined_message,
						capture_id, capture_pos)
				}
				value_message := '`${capture.value}` used as value'
				if !tc.errors.any(it.msg == value_message && it.pos.id == node.pos.id
					&& it.pos.offset == node.pos.offset && it.pos.end == node.pos.end) {
					tc.errors << tc.make_type_error_at(.return_mismatch, value_message, id,
						node.pos)
				}
			}
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
}

fn (tc &TypeChecker) lambda_capture_ident(id flat.NodeId, params map[string]bool, declarations map[string]flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident && !params[node.value] {
		if declaration_id := declarations[node.value] {
			if tc.a.node(declaration_id).pos.offset < node.pos.offset {
				return id
			}
		}
	}
	if node.kind == .lambda_expr {
		return none
	}
	for i in 0 .. node.children_count {
		if capture := tc.lambda_capture_ident(tc.a.child(node, i), params, declarations) {
			return capture
		}
	}
	return none
}

fn (mut tc TypeChecker) record_unused_top_level_vars(node flat.Node) {
	if tc.node_is_from_translated_file(node) {
		return
	}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .decl_assign {
			continue
		}
		for j := 0; j + 1 < child.children_count; j += 2 {
			lhs_id := tc.a.child(child, j)
			lhs := tc.a.node(lhs_id)
			if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_'
				|| lhs.value.starts_with('_') {
				continue
			}
			if tc.top_level_file_reads_ident(node, lhs.value, lhs_id) {
				continue
			}
			rhs_id := tc.a.child(child, j + 1)
			rhs := tc.a.node(rhs_id)
			generic_interface_cast := rhs.kind == .cast_expr
				&& (rhs.value in tc.interface_generic_params
				|| tc.qualify_name(rhs.value) in tc.interface_generic_params)
			if tc.expr_subtree_has_error(rhs_id) && !tc.expr_contains_nil_deref(rhs_id)
				&& !generic_interface_cast {
				continue
			}
			tc.record_warning_at(.unknown_ident, 'unused variable: `${lhs.value}`', lhs_id,
				tc.node_value_diagnostic_pos(lhs_id))
		}
	}
}

fn (tc &TypeChecker) expr_contains_nil_deref(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .prefix && node.op == .mul && node.children_count > 0
		&& tc.a.child_node(node, 0).kind == .nil_literal {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.expr_contains_nil_deref(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) top_level_file_reads_ident(node flat.Node, name string, decl_id flat.NodeId) bool {
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if is_top_level_statement_kind(tc.a.node(child_id).kind) {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		current := tc.a.node(id)
		if current.kind == .lambda_expr {
			continue
		}
		if id != decl_id && current.kind == .ident && current.value == name {
			return true
		}
		for i in 0 .. current.children_count {
			if i % 2 == 0 && current.kind == .decl_assign {
				continue
			}
			if i == 0 && current.kind == .assign {
				lhs := tc.a.child_node(current, i)
				if lhs.kind == .ident {
					continue
				}
			}
			stack << tc.a.child(current, i)
		}
	}
	return false
}

fn (tc &TypeChecker) expr_subtree_has_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	for diagnostic in tc.errors {
		if diagnostic.node == id {
			return true
		}
		if diagnostic.pos.id == root.pos.id && diagnostic.pos.offset >= root.pos.offset
			&& diagnostic.pos.end <= root.pos.end {
			return true
		}
	}
	mut stack := []flat.NodeId{}
	stack << id
	for stack.len > 0 {
		current_id := stack.pop()
		for diagnostic in tc.errors {
			if diagnostic.node == current_id {
				return true
			}
		}
		current := tc.a.node(current_id)
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	return false
}

fn (tc &TypeChecker) expr_subtree_has_undefined_ident_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	for diagnostic in tc.errors {
		if diagnostic.kind != .unknown_ident
			|| (!diagnostic.msg.starts_with('undefined variable:')
			&& !diagnostic.msg.starts_with('undefined ident:')) {
			continue
		}
		if diagnostic.node == id {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) expr_subtree_has_undefined_variable_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	for diagnostic in tc.errors {
		if diagnostic.kind != .unknown_ident || !diagnostic.msg.starts_with('undefined variable:') {
			continue
		}
		if diagnostic.node == id {
			return true
		}
		if diagnostic.pos.id == root.pos.id && diagnostic.pos.offset >= root.pos.offset
			&& diagnostic.pos.end <= root.pos.end {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) expr_subtree_has_no_value_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	return tc.errors.any(it.msg.contains('does not return a value') && (it.node == id
		|| (it.pos.id == root.pos.id && it.pos.offset >= root.pos.offset
		&& it.pos.end <= root.pos.end)))
}

fn (tc &TypeChecker) expr_subtree_has_error_except(id flat.NodeId, ignored TypeErrorKind) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	for diagnostic in tc.errors {
		if diagnostic.kind == ignored || diagnostic.msg == 'cannot type cast an Option'
			|| diagnostic.msg.starts_with('expression requires a non empty `or {}` block') {
			continue
		}
		if diagnostic.node == id {
			return true
		}
		if diagnostic.pos.id == root.pos.id && diagnostic.pos.offset >= root.pos.offset
			&& diagnostic.pos.end <= root.pos.end {
			return true
		}
	}
	mut stack := []flat.NodeId{}
	stack << id
	for stack.len > 0 {
		current_id := stack.pop()
		for diagnostic in tc.errors {
			if diagnostic.kind != ignored && diagnostic.msg != 'cannot type cast an Option'
				&& !diagnostic.msg.starts_with('expression requires a non empty `or {}` block')
				&& diagnostic.node == current_id {
				return true
			}
		}
		current := tc.a.node(current_id)
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	return false
}

fn (tc &TypeChecker) expr_subtree_has_map_void_return_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	return tc.errors.any(it.msg.starts_with('type mismatch, `')
		&& it.msg.ends_with('` does not return anything') && it.pos.id == root.pos.id
		&& it.pos.offset >= root.pos.offset && it.pos.end <= root.pos.end)
}

fn (tc &TypeChecker) expr_subtree_allows_unused_warning(id flat.NodeId) bool {
	if tc.expr_subtree_has_map_void_return_error(id) {
		return true
	}
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	return tc.errors.any(it.pos.id == root.pos.id && it.pos.offset >= root.pos.offset
		&& it.pos.end <= root.pos.end && (it.msg == 'map value cannot be only `none`'
		|| it.msg == 'cannot take the address of a literal value'
		|| it.msg.starts_with('ambiguous field `')
		|| it.msg.starts_with('invalid empty map initialisation syntax')
		|| it.msg.starts_with('invalid map value: expected ')
		|| (it.msg.starts_with('type mismatch, `') && it.msg.ends_with('` must return a bool'))
		|| (it.msg.contains('` is a generic fn, you should pass its concrete types, e.g. ')
		&& it.msg.ends_with('[int]')) || (it.msg.starts_with('generic struct `')
		&& it.msg.contains('` must specify type parameter'))))
}

fn (tc &TypeChecker) fn_body_read_names(node flat.Node, candidate_names map[string]bool) map[string]bool {
	leave_lambda := flat.NodeId(-2)
	mut used_names := map[string]bool{}
	mut shadow_depth := map[string]int{}
	mut shadow_names := []string{}
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		if id == leave_lambda {
			shadow_depth[shadow_names.pop()]--
			continue
		}
		current := tc.a.node(id)
		if current.kind == .lambda_expr {
			if tc.checker_fixture_mode {
				continue
			}
			if current.children_count > 0 {
				for i in 0 .. current.children_count - 1 {
					param := tc.a.child_node(current, i)
					if param.kind == .ident && candidate_names[param.value] {
						shadow_depth[param.value]++
						shadow_names << param.value
						stack << leave_lambda
					}
				}
				stack << tc.a.child(current, current.children_count - 1)
			}
			continue
		}
		if current.kind == .ident && candidate_names[current.value]
			&& shadow_depth[current.value] == 0 {
			used_names[current.value] = true
		}
		if current.kind in [.sql_expr, .comptime_if, .array_init] {
			for name, _ in candidate_names {
				if shadow_depth[name] > 0 || used_names[name] {
					continue
				}
				if (current.kind == .sql_expr && sql_text_contains_ident(current.value, name))
					|| (current.kind == .comptime_if
					&& type_text_contains_symbol(current.value, name)) {
					used_names[name] = true
					continue
				}
				if current.kind == .array_init {
					if bound := fixed_array_bound_text(current.typ) {
						if bound == name || type_text_contains_symbol(bound, name) {
							used_names[name] = true
						}
					}
				}
			}
		}
		if current.kind == .comptime_for && candidate_names[current.typ]
			&& shadow_depth[current.typ] == 0 {
			used_names[current.typ] = true
		}
		for i in 0 .. current.children_count {
			if i % 2 == 0 && current.kind == .decl_assign {
				lhs := tc.a.child_node(current, i)
				if lhs.kind == .ident {
					continue
				}
			}
			if i == 0 && current.kind == .assign && current.op == .assign {
				lhs := tc.a.child_node(current, i)
				if lhs.kind == .ident {
					continue
				}
			}
			stack << tc.a.child(current, i)
		}
	}
	return used_names
}

fn (mut tc TypeChecker) record_unused_fn_params(node flat.Node) {
	if tc.node_is_from_translated_file(node) || node.op == .arrow
		|| node.value in tc.a.disabled_fns
		|| (is_regular_v_test_file(tc.cur_file) && is_v_test_fn_name(node.value)) {
		return
	}
	for i in 0 .. node.children_count {
		param_id := tc.a.child(&node, i)
		param := tc.a.node(param_id)
		if param.kind != .param || param.op == .dot || param.value.len == 0 || param.value == '_'
			|| param.value.starts_with('_') {
			continue
		}
		if tc.fn_body_uses_ident(node, param.value) {
			continue
		}
		if tc.fn_body_reflects_param_type(node, param.typ) {
			continue
		}
		mut has_param_error := false
		for diagnostic in tc.errors {
			if diagnostic.node == param_id
				&& !diagnostic.msg.starts_with('duplicate of an import symbol ')
				&& !diagnostic.msg.starts_with('generic type name ')
				&& !diagnostic.msg.starts_with('invalid use of reserved type ') {
				if diagnostic.msg.starts_with('generic ')
					&& diagnostic.msg.contains(' in fn declaration must specify the generic type names') {
					continue
				}
				has_param_error = true
				break
			}
		}
		if has_param_error {
			continue
		}
		tc.record_notice_at(.unknown_ident, 'unused parameter: `${param.value}`', param_id,
			tc.node_value_diagnostic_pos(param_id))
	}
}

fn (tc &TypeChecker) fn_body_reflects_param_type(node flat.Node, param_type string) bool {
	if param_type.len == 0 || param_type !in node.generic_params() {
		return false
	}
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := tc.a.node(id)
		if child.kind == .comptime_for && child.typ == param_type {
			return true
		}
		for i in 0 .. child.children_count {
			stack << tc.a.child(child, i)
		}
	}
	return false
}

fn (mut tc TypeChecker) record_unused_fn_labels(node flat.Node) {
	mut labels := []flat.NodeId{}
	mut used := map[string]bool{}
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		current := tc.a.node(id)
		if current.kind == .label_stmt {
			labels << id
		} else if current.kind == .goto_stmt {
			used[current.value] = true
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	labels.sort(a < b)
	for label_id in labels {
		label := tc.a.node(label_id)
		if tc.label_starts_loop(label_id) {
			continue
		}
		if !used[label.value] {
			tc.record_warning_at(.unknown_ident, 'label `${label.value}` defined and not used',
				label_id, tc.a.node(label_id).pos)
		}
	}
}

fn (tc &TypeChecker) fn_body_uses_ident(node flat.Node, name string) bool {
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := tc.a.node(id)
		if child.kind == .ident && child.value == name {
			return true
		}
		if child.kind == .sql_expr && sql_text_contains_ident(child.value, name) {
			return true
		}
		for i in 0 .. child.children_count {
			stack << tc.a.child(child, i)
		}
	}
	return false
}

fn sql_text_contains_ident(text string, name string) bool {
	if name.len == 0 {
		return false
	}
	for token in text.split(' ') {
		if token == name {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) fn_has_deferred_generic_return(node flat.Node, generic_params map[string]bool) bool {
	mut last_stmt := flat.empty_node
	for i := int(node.children_count) - 1; i >= 0; i-- {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .param {
			continue
		}
		last_stmt = child_id
		break
	}
	if last_stmt == flat.empty_node {
		return false
	}
	if tc.stmt_is_generic_comptime_return(last_stmt, generic_params) {
		return true
	}
	mode := if tc.fn_context.return_type is ResultType {
		'!'
	} else if tc.fn_context.return_type is OptionType {
		'?'
	} else {
		return false
	}
	if !type_contains_unknown(tc.fn_context.return_type) {
		return false
	}
	return tc.stmt_is_terminal_void_propagation(last_stmt, mode)
}

fn (mut tc TypeChecker) stmt_is_generic_comptime_return(id flat.NodeId, generic_params map[string]bool) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .comptime_if
		|| !comptime_condition_references_generic_param(node.value, generic_params) {
		return false
	}
	// A returning branch only makes completeness specialization-dependent. The
	// monomorphizer rechecks the selected, pruned branch before emitting it.
	for i in 0 .. node.children_count {
		if tc.generic_comptime_branch_terminates(tc.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) generic_comptime_branch_terminates(id flat.NodeId) bool {
	if tc.stmt_definitely_returns(id) {
		return true
	}
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.block, .comptime_if {
			for i in 0 .. node.children_count {
				if tc.generic_comptime_branch_terminates(tc.a.child(&node, i)) {
					return true
				}
			}
		}
		.expr_stmt, .paren, .call {
			return tc.expr_never_returns_resolving(id)
		}
		else {}
	}
	return false
}

fn comptime_condition_references_generic_param(cond string, generic_params map[string]bool) bool {
	for param, _ in generic_params {
		mut offset := 0
		for offset + param.len <= cond.len {
			if cond[offset..offset + param.len] == param {
				before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
				after := offset + param.len
				after_ok := after >= cond.len || !comptime_cond_name_char(cond[after])
				if before_ok && after_ok {
					return true
				}
			}
			offset++
		}
	}
	return false
}

fn (mut tc TypeChecker) stmt_is_terminal_void_propagation(id flat.NodeId, mode string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .or_expr {
		return node.value == mode && tc.resolve_type(id) is Void
	}
	if node.kind in [.expr_stmt, .paren] && node.children_count == 1 {
		return tc.stmt_is_terminal_void_propagation(tc.a.child(&node, 0), mode)
	}
	return false
}

// prewarm_shared_type_cache forces the lazily-built type_cache indexes that
// fn-body checking commonly needs, so freezing the cache as the shared base
// hands every fork a complete index instead of each rebuilding its own.
fn (mut tc TypeChecker) prewarm_shared_type_cache() {
	if isnil(tc.type_cache) {
		return
	}
	_ = tc.local_fn_decl_exists('__v3_prewarm__')
	_ = tc.unique_qualified_type_name('__V3Prewarm__') or { '' }
	_ = tc.source_struct_has_non_builtin_error_embed('__V3Prewarm__', '', '')
}

// install_type_cache_overlay freezes the master's warm type cache as the
// shared read-only base for the parallel-check region and gives the master a
// private overlay, so its own writes cannot race worker reads of the base.
fn (mut tc TypeChecker) install_type_cache_overlay() {
	if isnil(tc.type_cache) {
		return
	}
	tc.prewarm_shared_type_cache()
	tc.type_cache = &TypeCache{
		base:          tc.type_cache
		parse_enabled: tc.type_cache.parse_enabled
	}
	if !isnil(tc.resolution_type_views) {
		// Cached parse views still point at the cache that is now the shared base.
		tc.reset_resolution_type_view_cache()
	}
}

// restore_type_cache_base folds the master's private overlay back into the
// frozen base once every thread has joined, and reattaches the base as the
// live cache (post-check phases mutate and invalidate it in place).
fn (mut tc TypeChecker) restore_type_cache_base() {
	if isnil(tc.type_cache) {
		return
	}
	mut overlay := tc.type_cache
	if isnil(overlay.base) {
		return
	}
	mut base := overlay.base
	base.parse_hits += overlay.parse_hits
	base.parse_misses += overlay.parse_misses
	base.c_hits += overlay.c_hits
	base.c_misses += overlay.c_misses
	for k, v in overlay.parse_entries {
		base.parse_entries[k] = v
	}
	for k, v in overlay.c_entries {
		base.c_entries[k] = v
	}
	for k, v in overlay.struct_field_entries {
		base.struct_field_entries[k] = v
	}
	for k, v in overlay.struct_field_misses {
		base.struct_field_misses[k] = v
	}
	for k, v in overlay.ierror_compat_entries {
		base.ierror_compat_entries[k] = v
	}
	for k, v in overlay.interface_impl_entries {
		base.interface_impl_entries[k] = v
	}
	if overlay.source_error_embed_indexed && !base.source_error_embed_indexed {
		base.source_error_embed_entries = overlay.source_error_embed_entries.move()
		base.source_error_embed_indexed = true
	}
	if overlay.ierror_impl_names_set && !base.ierror_impl_names_set {
		base.ierror_impl_names = overlay.ierror_impl_names
		base.ierror_impl_names_set = true
	}
	if overlay.short_type_name_index_built && !base.short_type_name_index_built {
		base.short_type_name_index = overlay.short_type_name_index.move()
		base.short_type_name_index_built = true
	}
	if overlay.local_fn_decl_indexed_len > base.local_fn_decl_indexed_len {
		base.local_fn_decl_index = overlay.local_fn_decl_index.move()
		base.local_fn_decl_indexed_len = overlay.local_fn_decl_indexed_len
		base.local_fn_decl_last_module = overlay.local_fn_decl_last_module
	}
	tc.type_cache = base
	if !isnil(tc.resolution_type_views) {
		tc.reset_resolution_type_view_cache()
	}
}

fn (tc &TypeChecker) fork_for_parallel_check() &TypeChecker {
	mut w := tc.fork_program_view(tc.a, map[int][]SymbolId{})
	// Parallel checker workers may populate this cache concurrently, so each
	// worker (and each disposable scoped batch) owns mutable result/miss maps while
	// sharing the declaration index that collect completed before checking starts.
	w.visible_mutation_cache = &VisibleMutationCache{
		decls:            tc.visible_mutation_cache.decls
		decl_misses:      map[string]bool{}
		results:          map[u64]bool{}
		decl_index_ready: tc.visible_mutation_cache.decl_index_ready
	}
	w.scope_parallel_check_workers = tc.scope_parallel_check_workers
	// The node-indexed cache arrays are intentionally SHARED with the master
	// (the fork copies the slice headers): each work item owns the disjoint
	// node id range [range_lo, fn_idx], and while parallel_check_sparse is set
	// a checker touches the shared arrays only for ids inside its current
	// item's range — no other thread reads or writes those slots. Everything
	// out of range goes through the private sparse maps below and is merged
	// after join.
	w.parallel_check_sparse = true
	w.check_range_lo = -1
	w.check_range_hi = -1
	w.sparse_resolved_call_names = map[int]string{}
	w.sparse_resolved_fn_values = map[int]string{}
	w.sparse_statement_nodes = map[int]bool{}
	w.sparse_expr_type_values = map[int]Type{}
	w.sparse_checking_nodes = map[int]bool{}
	w.method_values_by_fn = map[int][]string{}
	w.fn_context = new_function_check_context()
	w.generic_method_value_info = map[string]CallInfo{}
	w.smartcasts = map[string]Type{}
	w.ownership_time_ns = 0
	if check_memos_enabled() {
		w.import_info_cache = &ImportInfoCache{}
		w.qualify_name_cache = &QualifyNameCache{}
	}
	$if ownership ? {
		w.ownership_fork_for_parallel_check(tc)
	}
	w.type_cache = &TypeCache{
		// The master's frozen pre-region cache (the overlay's base) is shared
		// read-only across all forks; each fork writes to its own maps.
		base:                       if tc.type_cache != unsafe { nil } {
			tc.type_cache.base
		} else {
			&TypeCache(unsafe { nil })
		}
		parse_enabled:              if tc.type_cache != unsafe { nil } {
			tc.type_cache.parse_enabled
		} else {
			false
		}
		parse_entries:              map[u64]ParseTypeCacheEntry{}
		c_entries:                  map[TypeId]string{}
		struct_field_entries:       map[string]Type{}
		struct_field_misses:        map[string]bool{}
		ierror_compat_entries:      map[string]int{}
		source_error_embed_entries: map[string]int{}
	}
	if tc.scope_parallel_check_workers {
		// Shared interner growth from a helper arena would leave compilation-wide
		// tables pointing into freed storage. Scoped helpers therefore intern into
		// private tables and promote their compact results during merge.
		w.type_interner = new_type_interner()
		w.symbols = new_symbol_interner()
		w.type_cache.base = unsafe { nil }
	}
	return &w
}

struct CheckCloneChunkArgs {
	tc        voidptr
	items_ptr voidptr
mut:
	miss []int
}

// check_clone_chunk_thread promotes one chunk's node-cache payloads out of
// the (still alive) worker scopes: name clones land in the pool thread's
// persistent arena, and expr types are rebound via a read-only interner probe.
// Types the interner does not know yet are recorded and interned serially in
// chunk order afterwards, so interner ids match the serial merge exactly.
fn check_clone_chunk_thread(arg voidptr) voidptr {
	mut a := unsafe { &CheckCloneChunkArgs(arg) }
	mut tc := unsafe { &TypeChecker(a.tc) }
	items := unsafe { &[]CheckWorkItem(a.items_ptr) }
	for item in *items {
		for idx in item.range_lo .. item.fn_idx + 1 {
			if idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
				tc.resolved_call_names[idx] = tc.resolved_call_names[idx].clone()
			}
			if idx < tc.resolved_fn_value_set.len && tc.resolved_fn_value_set[idx] {
				tc.resolved_fn_value_names[idx] = tc.resolved_fn_value_names[idx].clone()
			}
			if idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
				if canonical := tc.probe_intern_type(tc.expr_type_values[idx]) {
					tc.expr_type_values[idx] = canonical
				} else {
					a.miss << idx
				}
			}
		}
	}
	return unsafe { nil }
}

fn (mut tc TypeChecker) intern_expr_type_misses(indexes []int) {
	for idx in indexes {
		_, canonical := tc.intern_type(clone_owned_type(tc.expr_type_values[idx]))
		tc.expr_type_values[idx] = canonical
	}
}

fn par_check_clone_enabled() bool {
	return os.getenv('V3_NO_PAR_CK_CLONE') == ''
}

fn (mut tc TypeChecker) clone_parallel_worker_node_caches(items []CheckWorkItem) {
	for item in items {
		for idx in item.range_lo .. item.fn_idx + 1 {
			if idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
				tc.resolved_call_names[idx] = tc.resolved_call_names[idx].clone()
			}
			if idx < tc.resolved_fn_value_set.len && tc.resolved_fn_value_set[idx] {
				tc.resolved_fn_value_names[idx] = tc.resolved_fn_value_names[idx].clone()
			}
			if idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
				_, canonical := tc.intern_type(clone_owned_type(tc.expr_type_values[idx]))
				tc.expr_type_values[idx] = canonical
			}
		}
	}
}

fn clone_parallel_type_error(err TypeError) TypeError {
	return TypeError{
		msg:        err.msg.clone()
		kind:       err.kind
		node:       err.node
		file:       err.file.clone()
		node_kind:  err.node_kind.clone()
		node_value: err.node_value.clone()
		node_pos:   err.node_pos.clone()
		pos:        err.pos
		details:    err.details.clone()
		severity:   err.severity.clone()
	}
}

fn clone_parallel_call_info(info CallInfo) CallInfo {
	return CallInfo{
		name:                 info.name.clone()
		params:               clone_owned_types(info.params)
		shared_params:        info.shared_params.clone()
		return_type:          clone_owned_type(info.return_type)
		has_receiver:         info.has_receiver
		is_variadic:          info.is_variadic
		is_c_variadic:        info.is_c_variadic
		params_known:         info.params_known
		has_implicit_veb_ctx: info.has_implicit_veb_ctx
		arg_offset:           info.arg_offset
	}
}

fn (mut tc TypeChecker) merge_parallel_check_worker(w &TypeChecker) {
	tc.merge_parallel_check_worker_scoped(w, false)
}

fn (mut tc TypeChecker) merge_parallel_check_worker_scoped(w &TypeChecker, scoped bool) {
	for err in w.errors {
		tc.errors << if scoped { clone_parallel_type_error(err) } else { err }
	}
	for notice in w.notices {
		tc.notices << if scoped { clone_parallel_type_error(notice) } else { notice }
	}
	for pending in w.pending_ierror_errors {
		tc.pending_ierror_errors << if scoped {
			PendingIerrorError{
				err:      clone_parallel_type_error(pending.err)
				fn_qname: pending.fn_qname.clone()
			}
		} else {
			pending
		}
	}
	if !isnil(tc.type_cache) && !isnil(w.type_cache) {
		tc.type_cache.parse_hits += w.type_cache.parse_hits
		tc.type_cache.parse_misses += w.type_cache.parse_misses
		tc.type_cache.c_hits += w.type_cache.c_hits
		tc.type_cache.c_misses += w.type_cache.c_misses
	}
	tc.ownership_time_ns += w.ownership_time_ns
	$if ownership ? {
		tc.ownership_merge_parallel_check_worker(w)
	}
	for idx, name in w.sparse_resolved_call_names {
		tc.resolved_call_names[idx] = if scoped { name.clone() } else { name }
		tc.resolved_call_set[idx] = true
	}
	for idx, name in w.sparse_resolved_fn_values {
		tc.resolved_fn_value_names[idx] = if scoped { name.clone() } else { name }
		tc.resolved_fn_value_set[idx] = true
	}
	for idx, _ in w.sparse_statement_nodes {
		tc.statement_nodes[idx] = true
	}
	for idx, typ in w.sparse_expr_type_values {
		if scoped {
			_, canonical := tc.intern_type(clone_owned_type(typ))
			tc.expr_type_values[idx] = canonical
		} else {
			tc.expr_type_values[idx] = typ
		}
		tc.expr_type_set[idx] = true
	}
	for fn_idx, dependencies in w.direct_dependencies_by_fn {
		mut merged := tc.direct_dependencies_by_fn[fn_idx] or { []SymbolId{} }
		for dependency in dependencies {
			owned_dependency := if scoped {
				id, _ := tc.intern_symbol(w.symbol_name(dependency))
				id
			} else {
				dependency
			}
			if owned_dependency !in merged {
				merged << owned_dependency
			}
		}
		if merged.len > 0 {
			tc.direct_dependencies_by_fn[fn_idx] = merged
		}
	}
	for fn_idx, values in w.method_values_by_fn {
		if values.len == 0 {
			continue
		}
		if fn_idx in tc.method_values_by_fn {
			for value in values {
				tc.method_values_by_fn[fn_idx] << if scoped { value.clone() } else { value }
			}
		} else {
			mut owned_values := []string{cap: values.len}
			for value in values {
				owned_values << if scoped { value.clone() } else { value }
			}
			tc.method_values_by_fn[fn_idx] = owned_values
		}
	}
	for key, info in w.generic_method_value_info {
		owned_key := if scoped { key.clone() } else { key }
		tc.generic_method_value_info[owned_key] = if scoped {
			clone_parallel_call_info(info)
		} else {
			info
		}
	}
}

fn (mut tc TypeChecker) free_parallel_check_worker_cache() {
	unsafe {
		if !isnil(tc.type_cache) {
			mut cache := tc.type_cache
			cache.parse_entries.free()
			cache.c_entries.free()
			cache.struct_field_entries.free()
			cache.struct_field_misses.free()
			cache.ierror_compat_entries.free()
			cache.interface_impl_entries.free()
			cache.source_error_embed_entries.free()
		}
	}
}
