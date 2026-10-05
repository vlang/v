module types

struct StorageQueryResult {
	writes map[string][]int
	guards map[u64]bool
}

@[heap]
struct StorageQueryTrace {
mut:
	guard_id u64
	guards   map[u64]bool
	complete bool = true
}

// A summary returns only paths and parameter indexes. Query state is disposable;
// memo entries retain the ancestor membership checks required to replay a result.
fn (tc &TypeChecker) param_storage_writes_for_decl(decl VisibleMutationFnDecl, target_param_idx int, mut visiting map[u64]bool) map[string][]int {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		defer { unsafe { prealloc_scope_end(scope) } }
		needs_analysis := if param := tc.visible_mutation_fn_param(decl, target_param_idx) {
			param.is_mut
		} else {
			false
		}
		if !needs_analysis {
			parent := unsafe { prealloc_scope_suspend(scope) }
			empty := map[string][]int{}
			unsafe { prealloc_scope_resume(scope, parent) }
			return empty
		}
		mut cache := tc.visible_mutation_cache
		use_cache := !isnil(cache) && cache.storage_query
		guard_id := visible_mutation_cache_id(decl, target_param_idx)
		if guard_id in visiting {
			parent := unsafe { prealloc_scope_suspend(scope) }
			if use_cache {
				cache.record_storage_query_guard(guard_id, true)
			}
			empty := map[string][]int{}
			unsafe { prealloc_scope_resume(scope, parent) }
			return empty
		}
		cache_key := '${decl.mod}:${decl.idx}:${target_param_idx}'
		if use_cache {
			mut lookup := cache
			for !isnil(lookup) && lookup.storage_query {
				for cached in lookup.storage_query_results[cache_key] {
					if storage_query_guards_match(cached.guards, visiting) {
						parent := unsafe { prealloc_scope_suspend(scope) }
						cache.record_storage_query_guards(cached.guards, true)
						unsafe { prealloc_scope_resume(scope, parent) }
						return cached.writes
					}
				}
				lookup = lookup.base
			}
		}
		mut view := tc.fork_storage_query_view()
		view.visible_mutation_cache.storage_query_trace = &StorageQueryTrace{
			guard_id: guard_id
			guards:   {
				guard_id: false
			}
		}
		mut active := visiting.clone()
		result := view.param_storage_writes_for_decl_unscoped(decl, target_param_idx, mut active)
		parent := unsafe { prealloc_scope_suspend(scope) }
		promoted := clone_storage_query_result(result)
		if use_cache {
			trace := view.visible_mutation_cache.storage_query_trace
			cache.record_storage_query_guards(trace.guards, trace.complete)
			if trace.complete {
				cache.cache_storage_query_result(cache_key, promoted, trace.guards, false)
			}
			for key, entries in view.visible_mutation_cache.storage_query_results {
				for cached in entries {
					// Retain completed descendant traces before this view's arena is freed.
					cache.cache_storage_query_result(key, cached.writes, cached.guards, true)
				}
			}
		}
		unsafe { prealloc_scope_resume(scope, parent) }
		return promoted
	} $else {
		return tc.param_storage_writes_for_decl_unscoped(decl, target_param_idx, mut visiting)
	}
}

fn storage_query_guards_match(guards map[u64]bool, visiting map[u64]bool) bool {
	for id, expected in guards {
		if (id in visiting) != expected { return false }
	}
	return true
}

fn (mut cache VisibleMutationCache) record_storage_query_guards(guards map[u64]bool, complete bool) {
	if isnil(cache.storage_query_trace) { return }
	mut trace := cache.storage_query_trace
	trace.complete = trace.complete && complete
	for id, present in guards {
		cache.record_storage_query_guard(id, present)
	}
}

fn (mut cache VisibleMutationCache) record_storage_query_guard(id u64, present bool) {
	if isnil(cache.storage_query_trace) { return }
	mut trace := cache.storage_query_trace
	// This declaration was pushed by the parent, rather than its incoming context.
	if id == trace.guard_id { return }
	if previous := trace.guards[id] {
		if previous != present { trace.complete = false }
	} else {
		trace.guards[id] = present
	}
}

fn clone_storage_query_result(result map[string][]int) map[string][]int {
	mut promoted := map[string][]int{}
	for path, sources in result { promoted[path.clone()] = sources.clone() }
	return promoted
}

fn (mut cache VisibleMutationCache) cache_storage_query_result(key string, result map[string][]int, guards map[u64]bool, clone_result bool) {
	if cache.storage_query_count >= 64 { return }
	mut entries := cache.storage_query_results[key] or { []StorageQueryResult{} }
	for entry in entries { if entry.guards == guards { return } }
	mut bytes := key.len + 64 + guards.len * 32
	for path, sources in result { bytes += path.len + sources.len * int(sizeof(int)) + 64 }
	if cache.storage_query_bytes + bytes > 256 * 1024 { return }
	entries << StorageQueryResult{
		writes: if clone_result { clone_storage_query_result(result) } else { result }
		guards: guards.clone()
	}
	cache.storage_query_results[key] = entries
	cache.storage_query_bytes += bytes
	cache.storage_query_count++
}

fn (tc &TypeChecker) fork_storage_query_view() &TypeChecker {
	mut view := tc.fork_program_view(tc.a, map[int][]SymbolId{})
	view.transform_signature_maps_shared = true
	view.transform_struct_maps_shared = true
	view.type_interner = new_type_interner()
	view.symbols = new_symbol_interner()
	view.fork_overlay = &TransformForkOverlay{
		base_node_count: -1
	}
	if !isnil(tc.fork_overlay) {
		view.fork_overlay.resolved_call_names = tc.fork_overlay.resolved_call_names.clone()
		view.fork_overlay.resolved_fn_values = tc.fork_overlay.resolved_fn_values.clone()
	}
	view.sparse_resolved_fn_values = tc.sparse_resolved_fn_values.clone()
	view.fork_fn_value_writes = tc.fork_fn_value_writes.clone()
	view.v_fn_semantic_names = tc.v_fn_semantic_names
	view.verbose = tc.verbose
	view.file_scope = tc.file_scope
	view.cur_scope = tc.cur_scope
	view.fn_context = clone_function_check_context(tc.fn_context)
	view.smartcasts = clone_smartcasts(tc.smartcasts)
	view.errors = tc.errors.clone()
	view.type_param_texts = tc.type_param_texts.clone()
	view.type_params_expanding = tc.type_params_expanding.clone()
	view.generic_decl_file = tc.generic_decl_file
	view.cur_fn_ret_type = tc.cur_fn_ret_type
	view.channel_send_or_expr_id = tc.channel_send_or_expr_id
	view.expected_expr_id = tc.expected_expr_id
	view.expected_expr_type = tc.expected_expr_type
	view.parallel_check_sparse = tc.parallel_check_sparse
	view.check_range_lo = tc.check_range_lo
	view.check_range_hi = tc.check_range_hi
	view.sparse_expr_type_values = tc.sparse_expr_type_values.clone()
	view.sparse_resolved_call_names = tc.sparse_resolved_call_names.clone()
	mut base := tc.type_cache
	if !isnil(base) && isnil(base.base) && base.local_fn_decl_indexed_len < tc.a.nodes.len {
		// A base cache freezes the declaration index. Keep cold-query AST scans.
		base = unsafe { nil }
	}
	view.type_cache = new_type_cache_with_base(!isnil(tc.type_cache) && tc.type_cache.parse_enabled,
		base)
	view.visible_mutation_cache = &VisibleMutationCache{
		base:          tc.visible_mutation_cache
		storage_query: true
	}
	return view
}
