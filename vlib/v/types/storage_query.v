module types

// A summary returns only paths and parameter indexes. Every other allocation
// belongs to this query, including its memoization and active recursion map.
fn (tc &TypeChecker) param_storage_writes_for_decl(decl VisibleMutationFnDecl, target_param_idx int, mut visiting map[u64]bool) map[string][]int {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		defer { unsafe { prealloc_scope_end(scope) } }
		mut cache := tc.visible_mutation_cache
		use_cache := !isnil(cache) && cache.storage_query
		mut cache_key := ''
		if use_cache {
			// Cycle cutoffs depend on the complete active set, not its order.
			mut ancestors := visiting.keys()
			ancestors.sort()
			cache_key = '${decl.mod}:${decl.idx}:${target_param_idx}:${ancestors}'
			if cached := cache.storage_query_results[cache_key] {
				return cached
			}
		}
		view := tc.fork_storage_query_view()
		mut active := visiting.clone()
		result := view.param_storage_writes_for_decl_unscoped(decl, target_param_idx, mut active)
		parent := unsafe { prealloc_scope_suspend(scope) }
		mut promoted := map[string][]int{}
		for path, sources in result {
			promoted[path.clone()] = sources.clone()
		}
		if use_cache && cache.storage_query_results.len < 64 {
			mut bytes := cache_key.len
			for path, sources in promoted {
				bytes += path.len + sources.len * int(sizeof(int)) + 64
			}
			if cache.storage_query_bytes + bytes <= 256 * 1024 {
				// Results remain read-only and belong to the immediate parent query.
				cache.storage_query_results[cache_key] = promoted
				cache.storage_query_bytes += bytes
			}
		}
		unsafe { prealloc_scope_resume(scope, parent) }
		return promoted
	} $else {
		return tc.param_storage_writes_for_decl_unscoped(decl, target_param_idx, mut visiting)
	}
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
