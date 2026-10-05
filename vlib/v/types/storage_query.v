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
		inherited_owner := use_cache && !isnil(cache.storage_query_owner)
		mut owner := if inherited_owner {
			cache.storage_query_owner
		} else {
			&VisibleMutationCache{ storage_query: true }
		}
		if inherited_owner {
			for cached in owner.storage_query_results[cache_key] {
				if storage_query_guards_match(cached.guards, visiting) {
					parent := unsafe { prealloc_scope_suspend(scope) }
					cache.record_storage_query_guards(cached.guards, true)
					unsafe { prealloc_scope_resume(scope, parent) }
					// The outer memo arena outlives every nested query; consumers only read it.
					return cached.writes
				}
			}
		}
		mut view := tc.fork_storage_query_view()
		view.visible_mutation_cache.storage_query_owner = owner
		mut scopes := if inherited_owner { cache.storage_query_scopes.clone() } else { []voidptr{} }
		scopes << scope
		view.visible_mutation_cache.storage_query_scopes = scopes
		view.visible_mutation_cache.storage_query_trace = &StorageQueryTrace{
			guard_id: guard_id
			guards:   {
				guard_id: false
			}
		}
		mut active := visiting.clone()
		result := view.param_storage_writes_for_decl_unscoped(decl, target_param_idx, mut active)
		trace := view.visible_mutation_cache.storage_query_trace
		// An active declaration also returns no paths, so this guard cannot affect an empty result.
		if trace.complete && result.len == 0 {
			trace.guards.delete(guard_id)
		}
		// Map iteration copies string keys, so estimate inside the disposable arena.
		estimated_bytes := if inherited_owner && trace.complete {
			storage_query_result_bytes(cache_key, result, trace.guards)
		} else {
			0
		}
		// Allocate suspension bookkeeping before selecting any older arena.
		mut states := []voidptr{len: if inherited_owner {
			cache.storage_query_scopes.len - 1
		} else {
			0
		}, init: voidptr(0)}
		parent := unsafe { prealloc_scope_suspend(scope) }
		if use_cache {
			cache.record_storage_query_guards(trace.guards, trace.complete)
		}
		if inherited_owner && trace.complete {
			// Only admitted memo payloads are allocated in the outer query's arena.
			suspend_storage_query_scopes(cache.storage_query_scopes, mut states)
			retained := owner.cache_storage_query_result(cache_key, result, trace.guards, true,
				estimated_bytes)
			resume_storage_query_scopes(cache.storage_query_scopes, states)
			if cached := retained {
				unsafe { prealloc_scope_resume(scope, parent) }
				return cached
			}
		}
		promoted := clone_storage_query_result(result)
		unsafe { prealloc_scope_resume(scope, parent) }
		return promoted
	} $else {
		return tc.param_storage_writes_for_decl_unscoped(decl, target_param_idx, mut visiting)
	}
}

fn suspend_storage_query_scopes(scopes []voidptr, mut states []voidptr) {
	$if prealloc {
		for i := scopes.len - 1; i > 0; i-- {
			states[i - 1] = unsafe { prealloc_scope_suspend(scopes[i]) }
		}
	}
}

fn resume_storage_query_scopes(scopes []voidptr, states []voidptr) {
	$if prealloc {
		for i in 1 .. scopes.len {
			unsafe {
				prealloc_scope_resume(scopes[i], states[i - 1])
			}
		}
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

fn storage_query_guards_equal(left map[u64]bool, right map[u64]bool) bool {
	if left.len != right.len { return false }
	for id, expected in left {
		actual := right[id] or { return false }
		if actual != expected { return false }
	}
	return true
}

fn storage_query_result_bytes(key string, result map[string][]int, guards map[u64]bool) int {
	mut bytes := key.len + 64 + guards.len * 32
	for path, sources in result { bytes += path.len + sources.len * int(sizeof(int)) + 64 }
	return bytes
}

fn (mut cache VisibleMutationCache) cache_storage_query_result(key string, result map[string][]int, guards map[u64]bool, clone_result bool, estimated_bytes int) ?map[string][]int {
	if cache.storage_query_count >= 4096
		|| cache.storage_query_bytes + estimated_bytes > 8 * 1024 * 1024 {
		return none
	}
	mut entries := cache.storage_query_results[key] or { []StorageQueryResult{} }
	for entry in entries {
		if storage_query_guards_equal(entry.guards, guards) { return entry.writes }
	}
	writes := if clone_result { clone_storage_query_result(result) } else { result }
	entries << StorageQueryResult{
		writes: writes
		guards: guards.clone()
	}
	cache.storage_query_results[key] = entries
	cache.storage_query_bytes += estimated_bytes
	cache.storage_query_count++
	return writes
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
