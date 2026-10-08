module types

import os
import v.flat
import v.parser
import v.pref

fn test_storage_observer_reads_completed_body_memo_without_rechecking_chained_receiver() {
	path := os.join_path(os.vtmp_dir(), 'v3_storage_observer_body_memo_${os.getpid()}.v')
	os.write_file(path, "struct Text { value string }
fn (text Text) view() string { return text.value }
fn make_text() Text { return Text{value: 'hello'} }
fn main() { _ = make_text().view() }
") or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	mut outer := flat.empty_node
	mut receiver := flat.empty_node
	for i, node in a.nodes {
		if node.kind == .call && node.children_count > 0 {
			callee := a.child_node(&node, 0)
			if callee.kind == .selector && callee.value == 'view' {
				outer = flat.NodeId(i)
				receiver = a.child(callee, 0)
			}
		}
	}
	assert tc.valid_node_id(outer) && tc.valid_node_id(receiver)
	tc.cur_module = 'main'
	tc.cur_file = path
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	info := tc.resolve_call_info(outer, *a.node(outer)) or { panic('missing call info') }
	typ := tc.resolve_type(receiver)
	assert info.name == 'Text.view'
	assert typ is Struct
	filled_before := tc.body_resolve_memo.filled.clone()
	checked_type_before := tc.body_resolve_memo.types[int(receiver) - tc.body_resolve_memo.lo]
	generation_before := tc.body_resolve_memo.call_generation
	mut probe := tc.fork_storage_observation_view()
	assert isnil(probe.body_resolve_memo)
	assert (probe.resolve_call_info(outer, *a.node(outer)) or { panic('missing observed call') }) == info
	assert probe.resolve_type(receiver) == Type(typ)
	assert !probe.storage_query_probe.recheck_requested
	assert tc.body_resolve_memo.filled == filled_before
	assert tc.body_resolve_memo.types[int(receiver) - tc.body_resolve_memo.lo] == checked_type_before
	assert tc.body_resolve_memo.call_generation == generation_before
	tc.body_resolve_memo.call_generation++
	assert probe.storage_query_probe_checked_call_info(outer) == none
	tc.body_resolve_memo.call_generation = generation_before
	slot := int(outer) & 2047
	tc.body_resolve_memo.call_ids[slot] = -1
	assert probe.storage_query_probe_checked_call_info(outer) == none
	tc.body_resolve_memo.call_ids[slot] = int(outer)
	mi := int(receiver) - tc.body_resolve_memo.lo
	tc.body_resolve_memo.filled[mi] = 0
	assert probe.storage_query_probe_checked_type(receiver) == none
	tc.body_resolve_memo.filled[mi] = filled_before[mi]
	mut changed := tc.fork_storage_observation_view()
	changed.expected_expr_id = int(outer)
	assert changed.storage_query_probe_checked_call_info(outer) == none
	assert changed.storage_query_probe_checked_type(receiver) == none
	mut different_scope := tc.fork_storage_observation_view()
	different_scope.cur_scope = &Scope{ ...tc.cur_scope }
	assert voidptr(different_scope.cur_scope) != voidptr(tc.cur_scope)
	assert *different_scope.cur_scope == *tc.cur_scope
	assert !different_scope.storage_query_probe_facts_unchanged()
	assert different_scope.storage_query_probe_checked_call_info(outer) == none
	assert different_scope.storage_query_probe_checked_type(receiver) == none
	mut cold := tc.fork_storage_observation_view()
	cold.remember_expr_type(receiver, Type(String{}))
	assert cold.storage_query_probe_checked_call_info(outer) == none
	_ = cold.resolve_call_info(outer, *a.node(outer))
	assert cold.storage_query_probe.recheck_requested
	assert tc.errors.len == 0
	assert tc.body_resolve_memo.filled == filled_before
	assert tc.body_resolve_memo.types[int(receiver) - tc.body_resolve_memo.lo] == checked_type_before
}

fn test_storage_observers_preserve_annotation_reads_and_private_nested_writes() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'source')
	mut tc := TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	tc.check_range_lo = -1
	tc.check_range_hi = -1
	tc.sparse_expr_type_values[int(id)] = Type(bool_)
	tc.sparse_resolved_call_names[int(id)] = 'main.original_call'
	tc.sparse_resolved_fn_values[int(id)] = 'main.original_value'
	tc.fork_fn_value_writes[int(id)] = 'main.original_write'
	tc.fork_overlay = &TransformForkOverlay{
		resolved_call_names: {
			int(id): 'main.overlay_call'
		}
		resolved_fn_values:  {
			int(id): 'main.overlay_value'
		}
	}
	mut exact := tc.fork_storage_query_view()
	mut outer := tc.fork_storage_observation_view()
	assert exact.sparse_expr_type_values == tc.sparse_expr_type_values
	assert exact.sparse_resolved_call_names == tc.sparse_resolved_call_names
	assert exact.sparse_resolved_fn_values == tc.sparse_resolved_fn_values
	assert exact.fork_fn_value_writes == tc.fork_fn_value_writes
	assert exact.fork_overlay.resolved_call_names == tc.fork_overlay.resolved_call_names
	assert exact.fork_overlay.resolved_fn_values == tc.fork_overlay.resolved_fn_values
	assert outer.sparse_expr_type_values.len == 0 && outer.sparse_resolved_call_names.len == 0
	assert outer.sparse_resolved_fn_values.len == 0 && outer.fork_fn_value_writes.len == 0
	assert outer.fork_overlay.resolved_call_names.len == 0 && outer.fork_overlay.resolved_fn_values.len == 0
	assert outer.cached_expr_type(id) == tc.cached_expr_type(id)
	assert outer.cached_resolved_call(id) == tc.cached_resolved_call(id)
	assert outer.resolved_fn_value_name(id) == tc.resolved_fn_value_name(id)
	outer.sparse_expr_type_values[int(id)] = Type(string_)
	outer.sparse_resolved_call_names[int(id)] = 'main.private_call'
	outer.sparse_resolved_fn_values[int(id)] = 'main.private_value'
	outer.fork_fn_value_writes[int(id)] = 'main.private_write'
	outer.fork_overlay.resolved_call_names[int(id)] = 'main.private_overlay_call'
	outer.fork_overlay.resolved_fn_values[int(id)] = 'main.private_overlay_value'
	mut nested := outer.fork_storage_observation_view()
	assert nested.cached_expr_type(id) == outer.cached_expr_type(id)
	assert nested.cached_resolved_call(id) == outer.cached_resolved_call(id)
	assert nested.resolved_fn_value_name(id) == outer.resolved_fn_value_name(id)
	nested.sparse_expr_type_values[int(id)] = Type(int_)
	nested.sparse_resolved_call_names[int(id)] = 'main.nested_call'
	nested.sparse_resolved_fn_values[int(id)] = 'main.nested_value'
	nested.fork_fn_value_writes[int(id)] = 'main.nested_write'
	nested.fork_overlay.resolved_call_names[int(id)] = 'main.nested_overlay_call'
	nested.fork_overlay.resolved_fn_values[int(id)] = 'main.nested_overlay_value'
	assert (outer.sparse_expr_type_values[int(id)] or { panic('missing private type') }) == Type(string_)
	assert outer.sparse_resolved_call_names[int(id)] == 'main.private_call'
	assert outer.sparse_resolved_fn_values[int(id)] == 'main.private_value'
	assert outer.fork_fn_value_writes[int(id)] == 'main.private_write'
	assert outer.fork_overlay.resolved_call_names[int(id)] == 'main.private_overlay_call'
	assert outer.fork_overlay.resolved_fn_values[int(id)] == 'main.private_overlay_value'
	exact.sparse_expr_type_values[int(id)] = Type(int_)
	exact.fork_overlay.resolved_call_names[int(id)] = 'main.exact_override'
	assert (tc.sparse_expr_type_values[int(id)] or { panic('missing original type') }) == Type(bool_)
	assert tc.fork_overlay.resolved_call_names[int(id)] == 'main.overlay_call'
}

fn test_potential_binding_sources_include_exact_conditional_and_rebound_sources() {
	path := os.join_path(os.vtmp_dir(), 'v3_potential_binding_sources_${os.getpid()}.v')
	os.write_file(path, '@[heap]
struct Value {}
struct Box { mut: value &Value }
fn store(mut target Box, source &Value) { target.value = source }
fn inspect(mut target Box, source &Value) { _ = target; _ = source }
fn history(mut target Box, first &Value, replacement &Value, flag bool) &Value {
	store(mut target, first)
	target = Box{value: replacement}
	if flag { store(mut target, first) }
	inspect(mut target, replacement)
	return target.value
}
fn main() {
	first := &Value{}
	replacement := &Value{}
	mut target := Box{value: first}
	_ = history(mut target, first, replacement, false)
}
') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	mut decl_idx := -1
	mut use_id := flat.empty_node
	for i, node in a.nodes {
		if node.kind == .fn_decl && node.value == 'history' { decl_idx = i }
		if node.kind == .selector && node.value == 'value' {
			base_id := a.child(&node, 0)
			if a.node(base_id).value == 'target' { use_id = base_id }
		}
	}
	assert decl_idx >= 0 && int(use_id) >= 0
	decl := VisibleMutationFnDecl{ idx: decl_idx, mod: 'main' }
	exact := tc.visible_fn_local_binding_rhs_before(decl, 'target', use_id)
	mut probe := tc.fork_storage_observation_view()
	potential := probe.visible_fn_local_binding_sources_before(decl, 'target', use_id, true)
	assert exact.len > 0
	assert potential.len > exact.len
	for source in exact {
		assert source in potential
	}
	for source in potential {
		assert tc.valid_node_id(source)
	}
	for i, _ in a.nodes {
		id := flat.NodeId(i)
		assert probe.cached_expr_type(id) == tc.cached_expr_type(id)
		assert probe.cached_resolved_call(id) == tc.cached_resolved_call(id)
		assert probe.resolved_fn_value_name(id) == tc.resolved_fn_value_name(id)
		assert probe.is_statement_node(id) == tc.is_statement_node(id)
	}
	mut parallel := tc.fork_storage_query_view()
	parallel.parallel_check_sparse = true
	parallel.check_range_lo = decl_idx
	parallel.check_range_hi = int(use_id)
	parallel.sparse_expr_type_values[int(use_id)] = Type(String{})
	parallel.fork_overlay.resolved_fn_values[int(use_id)] = 'main.original_callback'
	mut parallel_probe := parallel.fork_storage_observation_view()
	parallel_potential := parallel_probe.visible_fn_local_binding_sources_before(decl, 'target', use_id, true)
	for source in parallel.visible_fn_local_binding_rhs_before(decl, 'target', use_id) {
		assert source in parallel_potential
	}
	for i, _ in a.nodes {
		id := flat.NodeId(i)
		assert parallel_probe.cached_expr_type(id) == parallel.cached_expr_type(id)
		assert parallel_probe.cached_resolved_call(id) == parallel.cached_resolved_call(id)
		assert parallel_probe.resolved_fn_value_name(id) == parallel.resolved_fn_value_name(id)
		assert parallel_probe.is_statement_node(id) == parallel.is_statement_node(id)
	}
	parallel_probe.set_resolved_fn_value(int(use_id), 'main.private_callback')
	assert (parallel_probe.resolved_fn_value_name(use_id) or { panic('missing callback') }) == 'main.private_callback'
	assert (parallel.resolved_fn_value_name(use_id) or { panic('missing callback') }) == 'main.original_callback'
	parallel_probe.clear_resolved_fn_value(use_id)
	assert parallel_probe.resolved_fn_value_name(use_id) == none
	assert (parallel.resolved_fn_value_name(use_id) or { panic('missing callback') }) == 'main.original_callback'
	original_type := tc.cached_expr_type(use_id)
	probe.remember_expr_type(use_id, Type(String{}))
	assert (probe.cached_expr_type(use_id) or { panic('missing type') }) == Type(String{})
	assert tc.cached_expr_type(use_id) == original_type
	probe.trust_checked_expr_types = true
	assert probe.resolve_type(use_id) == Type(String{})
	probe.mark_statement_context(use_id)
	assert probe.is_statement_node(use_id)
	assert tc.is_statement_node(use_id) == false
	mut call_id := flat.empty_node
	for i, _ in a.nodes {
		if tc.cached_resolved_call(flat.NodeId(i)) != none {
			call_id = flat.NodeId(i)
			break
		}
	}
	assert tc.valid_node_id(call_id)
	original_call := tc.cached_resolved_call(call_id)
	probe.remember_resolved_call(call_id, 'main.private_override')
	assert (probe.cached_resolved_call(call_id) or { panic('missing call') }) == 'main.private_override'
	assert tc.cached_resolved_call(call_id) == original_call
	mut nested := probe.fork_storage_query_view()
	nested.begin_sparse_transform_node_caches(a.nodes.len)
	nested.remember_expr_type(use_id, Type(bool_))
	nested.remember_resolved_call(call_id, 'main.nested_override')
	nested.mark_statement_context(use_id)
	assert (nested.cached_expr_type(use_id) or { panic('missing type') }) == Type(bool_)
	assert (nested.cached_resolved_call(call_id) or { panic('missing call') }) == 'main.nested_override'
	assert nested.is_statement_node(use_id)
	assert tc.cached_expr_type(use_id) == original_type
	assert tc.cached_resolved_call(call_id) == original_call
	assert !tc.is_statement_node(use_id)
	mut bounded := tc.fork_storage_observation_view()
	mut bounded_child := bounded.fork_storage_query_view()
	bounded.storage_query_probe.value_observations = storage_query_probe_max_observations - 1
	assert bounded.storage_query_probe_can_observe()
	assert !bounded_child.storage_query_probe_can_observe()
	assert bounded.storage_query_probe.uncertain
	assert tc.storage_query_probe == unsafe { nil }
	errors_before := tc.errors.clone()
	types_before := tc.expr_type_values.clone()
	set_before := tc.expr_type_set.clone()
	scope_before := tc.cur_scope.names.clone()
	$if ownership ? {
		mut state := tc.ownership_state()
		state.borrowed_vars['probe_source'] = [BorrowInfo{ borrower: 'holder.target', pos: use_id }]
		state.ownership_fn_return_param_descs['main.probe_alias'] = [OwnershipReturnParamDescendant{ param_idx: 1, via: ['main.origin'] }]
		mut isolated := tc.fork_storage_observation_view()
		mut lookup := isolated.fork_type_parse_view(tc.cur_file, tc.cur_module)
		mut child_observer := isolated.fork_storage_observation_view()
		assert voidptr(child_observer.ownership) == voidptr(isolated.ownership)
		assert voidptr(child_observer.ownership) != voidptr(state)
		// Borrow retained descriptors so these mutations test backing-storage isolation.
		mut loans := unsafe { lookup.ownership.borrowed_vars['probe_source'] }
		loans[0] = BorrowInfo{ borrower: 'changed', pos: use_id }
		mut via := unsafe { lookup.ownership.ownership_fn_return_param_descs['main.probe_alias'][0].via }
		via[0] = 'main.changed'
		assert isolated.ownership.borrowed_vars['probe_source'][0].borrower == 'changed'
		assert isolated.ownership.ownership_fn_return_param_descs['main.probe_alias'][0].via == ['main.changed']
		assert state.borrowed_vars['probe_source'][0].borrower == 'holder.target'
		assert state.ownership_fn_return_param_descs['main.probe_alias'][0].via == ['main.origin']
		mut scalar_id := flat.empty_node
		for i, node in a.nodes {
			if node.kind == .bool_literal {
				scalar_id = flat.NodeId(i)
				break
			}
		}
		assert tc.valid_node_id(scalar_id)
		isolated.storage_query_probe.value_observations = storage_query_probe_max_observations - 1
		assert isolated.returned_receiver_local_storage_in_value(scalar_id, []flat.NodeId{}, map[string]flat.NodeId{}) == none
		assert !isolated.storage_query_probe.uncertain
		assert child_observer.returned_receiver_local_storage_in_value(scalar_id, []flat.NodeId{}, map[string]flat.NodeId{}) == none
		assert isolated.storage_query_probe.uncertain
		assert !isolated.returned_binding_history_has_no_local_sources(use_id, []flat.NodeId{}, map[string]flat.NodeId{})
		assert !isolated.storage_query_probe.recheck_requested
		lookup.check_node(use_id)
		assert isolated.storage_query_probe.recheck_requested
		assert tc.storage_query_probe == unsafe { nil }
	}
	nested.check_node(use_id)
	assert probe.storage_query_probe.recheck_requested
	assert tc.storage_query_probe == unsafe { nil }
	assert tc.errors == errors_before
	assert tc.expr_type_values == types_before
	assert tc.expr_type_set == set_before
	assert tc.cur_scope.names == scope_before
	tc.lexical_smartcast_misses[int(use_id)] = false
	mut cold := tc.fork_storage_observation_view()
	mut narrowed := cold.fork_smartcast_query_view()
	assert narrowed.smartcast_type(use_id) == none
	assert !tc.lexical_smartcast_misses[int(use_id)]
	assert narrowed.type_cache.lexical_smartcast_misses[int(use_id)]
	mut memo := tc.fork_storage_observation_view()
	key := memo.storage_query_probe_history_key(use_id, [use_id, call_id], map[string]flat.NodeId{}) or { panic('missing history key') }
	permuted := memo.storage_query_probe_history_key(use_id, [call_id, use_id, call_id], map[string]flat.NodeId{}) or { panic('missing history key') }
	assert key == permuted
	different := memo.storage_query_probe_history_key(use_id, [call_id], map[string]flat.NodeId{}) or { panic('missing history key') }
	assert key != different
	assert memo.storage_query_probe_history_key(use_id, []flat.NodeId{}, {
		'source': call_id
	}) == none
	memo.remember_expr_type(call_id, Type(String{}))
	assert memo.storage_query_probe_history_key(use_id, [use_id, call_id], map[string]flat.NodeId{}) == none
	mut uncertain := tc.fork_storage_observation_view()
	uncertain.storage_query_probe.uncertain = true
	assert uncertain.storage_query_probe_history_key(use_id, [use_id, call_id], map[string]flat.NodeId{}) == none
	mut memo_scope := voidptr(0)
	$if prealloc {
		memo_scope = unsafe { prealloc_scope_begin() }
	}
	defer {
		$if prealloc {
			unsafe { prealloc_scope_end(memo_scope) }
		}
	}
	mut certificates := tc.fork_storage_observation_view()
	if memo_scope != unsafe { nil } { certificates.storage_query_probe_scopes = [memo_scope] }
	first_prunes := certificates.storage_query_probe.prune_observations
	certificates.storage_query_probe_note_pruned_ancestors([int(call_id)])
	certificates.storage_query_probe_remember_history(use_id, [call_id], map[string]flat.NodeId{}, false, first_prunes)
	assert certificates.storage_query_probe_cached_history(use_id, [call_id, use_id], map[string]flat.NodeId{})
	assert !certificates.storage_query_probe_cached_history(use_id, []flat.NodeId{}, map[string]flat.NodeId{})
	assert !certificates.storage_query_probe_cached_history(use_id, [use_id], map[string]flat.NodeId{})
	certificates.storage_query_probe_remember_history(call_id, [call_id], map[string]flat.NodeId{}, true, certificates.storage_query_probe.prune_observations)
	assert !certificates.storage_query_probe_cached_history(call_id, [call_id, use_id], map[string]flat.NodeId{})
	references_before := certificates.storage_query_probe.unguarded_reference_observations
	assert certificates.storage_query_probe_cached_history(call_id, [call_id], map[string]flat.NodeId{})
	assert certificates.storage_query_probe.unguarded_reference_observations > references_before
	certificates.storage_query_probe_remember_history(flat.NodeId(decl_idx), [call_id], map[string]flat.NodeId{}, certificates.storage_query_probe.unguarded_reference_observations != references_before, certificates.storage_query_probe.prune_observations)
	assert !certificates.storage_query_probe_cached_history(flat.NodeId(decl_idx), [
		call_id,
		use_id,
	], map[string]flat.NodeId{})
	// Unused incoming ancestors do not constrain a silent-cycle certificate.
	irrelevant_id := flat.NodeId(decl_idx + 1)
	inner_prunes := certificates.storage_query_probe.prune_observations
	certificates.storage_query_probe_note_pruned_ancestors([int(call_id)])
	certificates.storage_query_probe_remember_history(irrelevant_id, [call_id, use_id], map[string]flat.NodeId{}, false, inner_prunes)
	assert certificates.storage_query_probe_cached_history(irrelevant_id, [call_id], map[string]flat.NodeId{})
	assert !certificates.storage_query_probe_cached_history(irrelevant_id, [use_id], map[string]flat.NodeId{})
	// A cached child's required ancestors remain requirements of its parent.
	parent_id := flat.NodeId(decl_idx + 2)
	parent_prunes := certificates.storage_query_probe.prune_observations
	assert certificates.storage_query_probe_cached_history(irrelevant_id, [call_id, use_id], map[string]flat.NodeId{})
	certificates.storage_query_probe_remember_history(parent_id, [call_id, use_id], map[string]flat.NodeId{}, false, parent_prunes)
	assert certificates.storage_query_probe_cached_history(parent_id, [call_id], map[string]flat.NodeId{})
	assert !certificates.storage_query_probe_cached_history(parent_id, []flat.NodeId{}, map[string]flat.NodeId{})
	// Earlier prune observations and internally added ancestors do not constrain this proof.
	fresh_id := flat.NodeId(decl_idx + 3)
	fresh_prunes := certificates.storage_query_probe.prune_observations
	certificates.storage_query_probe_note_pruned_ancestors([int(fresh_id)])
	certificates.storage_query_probe_remember_history(fresh_id, [call_id, use_id], map[string]flat.NodeId{}, false, fresh_prunes)
	assert certificates.storage_query_probe_cached_history(fresh_id, []flat.NodeId{}, map[string]flat.NodeId{})
	mut exhausted_tracking := tc.fork_storage_observation_view()
	exhausted_tracking.storage_query_probe_scopes = certificates.storage_query_probe_scopes.clone()
	exhausted_tracking.storage_query_probe.prune_observations = max_u64
	exhausted_tracking.storage_query_probe_note_pruned_ancestors([int(call_id)])
	assert exhausted_tracking.storage_query_probe.uncertain
	exhausted_tracking.storage_query_probe_remember_history(use_id, [call_id], map[string]flat.NodeId{}, false, max_u64)
	assert !exhausted_tracking.storage_query_probe_cached_history(use_id, []flat.NodeId{}, map[string]flat.NodeId{})
	mut changed := certificates.fork_storage_query_view()
	changed.expected_expr_id = int(use_id)
	assert !changed.storage_query_probe_cached_history(use_id, [call_id, use_id], map[string]flat.NodeId{})
	changed = certificates.fork_storage_query_view()
	changed.cur_scope = &Scope{ ...tc.cur_scope }
	assert voidptr(changed.cur_scope) != voidptr(tc.cur_scope)
	assert *changed.cur_scope == *tc.cur_scope
	assert !changed.storage_query_probe_cached_history(use_id, [call_id, use_id], map[string]flat.NodeId{})
	changed = certificates.fork_storage_query_view()
	changed.remember_expr_type(call_id, Type(String{}))
	assert !changed.storage_query_probe_cached_history(use_id, [call_id, use_id], map[string]flat.NodeId{})
	$if prealloc {
		inner := unsafe { prealloc_scope_begin() }
		mut inner_view := certificates.fork_storage_query_view()
		inner_view.storage_query_probe_scopes = certificates.storage_query_probe_scopes.clone()
		inner_view.storage_query_probe_scopes << inner
		retained_prunes := inner_view.storage_query_probe.prune_observations
		inner_view.storage_query_probe_note_pruned_ancestors([int(use_id)])
		inner_view.storage_query_probe_remember_history(flat.NodeId(decl_idx), [use_id], map[string]flat.NodeId{}, false, retained_prunes)
		unsafe { prealloc_scope_end(inner) }
		assert certificates.storage_query_probe.pruned_ancestors[int(use_id)] > retained_prunes
		assert certificates.storage_query_probe_cached_history(flat.NodeId(decl_idx), [
			use_id,
			call_id,
		], map[string]flat.NodeId{})
	}
	certificates.storage_query_probe.recheck_requested = true
	assert !certificates.storage_query_probe_cached_history(use_id, [call_id], map[string]flat.NodeId{})
}

fn test_param_storage_sources_distinguish_interpolation_from_captures_and_side_effects() {
	path := os.join_path(os.vtmp_dir(), 'v3_interpolated_storage_sources_${os.getpid()}.v')
	os.write_file(path, 'struct Payload {
	value int
	text string
}

struct Holder {
mut:
	text string
	target &Payload
	callback fn () string = unsafe { nil }
}

fn store_formatted(mut holder Holder, source &Payload) {
	holder.text = "value:\${source.value}"
}

fn store_single_string(mut holder Holder, source &Payload) {
	holder.text = "\${source.text}"
}

fn store_single_formatted_string(mut holder Holder, source &Payload) {
	holder.text = "\${source.text:1s}"
}

fn store_direct(mut holder Holder, source &Payload) {
	holder.target = source
}

fn write_and_value(mut holder Holder, source &Payload) int {
	holder.target = source
	return source.value
}

fn store_formatted_with_write(mut holder Holder, source &Payload) {
	holder.text = "value:\${write_and_value(mut holder, source)}"
}

fn store_formatted_callback(mut holder Holder, source &Payload) {
	holder.callback = fn [source] () string {
		return "value:\${source.value}"
	}
}

fn store_formatted_lambda(mut holder Holder, source &Payload) {
	holder.callback = || "value:\${source.value}"
}

fn main() {
	source := &Payload{value: 42}
	mut holder := Holder{target: source}
	store_formatted(mut holder, source)
	store_single_string(mut holder, source)
	store_single_formatted_string(mut holder, source)
	store_direct(mut holder, source)
	store_formatted_with_write(mut holder, source)
	store_formatted_callback(mut holder, source)
	store_formatted_lambda(mut holder, source)
}
') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	mut calls := map[string]flat.NodeId{}
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		for wanted in ['store_formatted', 'store_direct', 'store_formatted_with_write',
			'store_formatted_callback', 'store_formatted_lambda', 'store_single_string',
			'store_single_formatted_string'] {
			if name.ends_with(wanted) {
				calls[wanted] = flat.NodeId(i)
			}
		}
	}
	assert calls.len == 7, calls.str()
	assert tc.call_param_storage_source_params(calls['store_formatted'], 0) == []
	assert tc.call_param_storage_source_params(calls['store_direct'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_formatted_with_write'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_formatted_callback'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_formatted_lambda'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_single_string'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_single_formatted_string'], 0) == [1]
}

fn test_param_storage_sources_include_all_delegated_mutable_targets() {
	path := os.join_path(os.vtmp_dir(), 'v3_delegated_storage_sources_${os.getpid()}.v')

	os.write_file(path, 'struct Value {}

struct Box {
mut:
	value &Value
}

struct Pair {
mut:
	first  &Box
	second &Box
}

struct IndexedBox {
mut:
	values []&Value
}

struct BoxHolder {
	mut:
	box &Box
}

fn route_targets(mut first &Box, mut second &Box, value &Value) {
	_ = first
	second.value = value
}

fn set_pair_sources(mut pair Pair, value &Value) {
	route_targets(mut pair.first, mut pair.second, value)
}

fn pick_index() int {
	return 0
}

fn store_at_computed_index(mut box IndexedBox, value &Value) {
	box.values[pick_index()] = value
}

fn copy_then_store(mut box Box, value &Value) {
	mut copy := box
	copy.value = value
}

fn store_through_aggregate(mut box &Box, value &Value) {
	tmp := Box{
		value: unsafe { value }
	}
	box.value = tmp.value
}

fn store_through_target_aggregate(mut box Box, value &Value) {
	mut holder := BoxHolder{
		box: unsafe { &box }
	}
	holder.box.value = value
}

fn store_deferred(mut box &Box, value &Value, replacement &Value) {
	defer {
		box.value = value
	}
	box.value = replacement
}

fn store_selected(mut box &Box, value &Value, replacement &Value, signal chan bool) {
	select {
		<-signal {
			box.value = value
		}
		else {
			box.value = replacement
		}
	}
}

fn set_or_reset(mut box &Box, value &Value, replacement &Value, stop bool) {
	box.value = value
	if stop {
		return
	}
	box.value = replacement
}

fn store_value(mut box &Box, value &Value) {
	box.value = value
}

fn delegate_then_reset(mut box &Box, value &Value, replacement &Value) {
	store_value(mut box, value)
	box.value = replacement
}

fn reset_then_delegate(mut box &Box, value &Value, replacement &Value) {
	box.value = value
	store_value(mut box, replacement)
}

fn keep_value(mut box &Box) {
	box.value = box.value
}

fn set_then_keep(mut box &Box, value &Value) {
	box.value = value
	keep_value(mut box)
}

fn store_in_loop(mut box &Box, value &Value, replacement &Value, stop bool) {
	for {
		box.value = value
		if stop {
			break
		}
		box.value = replacement
		break
	}
}

fn may_fail(ok bool) ! {
	if !ok {
		return error("failed")
	}
}

fn store_or_replace(mut box &Box, value &Value, replacement &Value, ok bool) {
	box.value = value
	may_fail(ok) or {
		box.value = replacement
	}
}

fn main() {
	mut value := Value{}
	mut first := &Box{
		value: &value
	}
	mut second := &Box{
		value: &value
	}
	mut pair := Pair{
		first: first
		second: second
	}
	mut indexed := IndexedBox{
		values: [&value]
	}
	mut copied := Box{
		value: &value
	}
	set_pair_sources(mut pair, &value)
	store_at_computed_index(mut indexed, &value)
	copy_then_store(mut copied, &value)
	store_through_aggregate(mut first, &value)
	store_through_target_aggregate(mut copied, &value)
	store_deferred(mut first, &value, &value)
	store_selected(mut first, &value, &value, chan bool{})
	set_or_reset(mut first, &value, &value, true)
	delegate_then_reset(mut second, &value, &value)
	reset_then_delegate(mut second, &value, &value)
	set_then_keep(mut second, &value)
	store_in_loop(mut first, &value, &value, true)
	store_or_replace(mut second, &value, &value, true)
}
') or { panic(err) }
	defer {
		os.rm(path) or {}
	}

	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut calls := map[string]flat.NodeId{}
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		for wanted in ['set_pair_sources', 'store_at_computed_index', 'copy_then_store',
			'store_through_aggregate', 'store_through_target_aggregate', 'store_deferred',
			'store_selected', 'set_or_reset', 'delegate_then_reset', 'reset_then_delegate',
			'set_then_keep', 'store_in_loop', 'store_or_replace'] {
			if name.ends_with(wanted) {
				calls[wanted] = flat.NodeId(i)
			}
		}
	}
	assert calls.len == 13, calls.str()
	assert tc.call_param_storage_source_params(calls['set_pair_sources'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_at_computed_index'], 0) == [
		1,
	]
	assert tc.call_param_storage_source_params(calls['copy_then_store'], 0) == []
	assert tc.call_param_storage_source_params(calls['store_through_aggregate'], 0) == [
		1,
	]
	assert tc.call_param_storage_source_params(calls['store_through_target_aggregate'], 0) == [
		1,
	]
	assert tc.call_param_storage_source_params(calls['store_deferred'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_selected'], 0) == [1, 2]
	assert tc.call_param_storage_source_params(calls['set_or_reset'], 0) == [1, 2]
	assert tc.call_param_storage_source_params(calls['delegate_then_reset'], 0) == [2]
	assert tc.call_param_storage_source_params(calls['reset_then_delegate'], 0) == [2]
	assert tc.call_param_storage_source_params(calls['set_then_keep'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_in_loop'], 0) == [1, 2]
	assert tc.call_param_storage_source_params(calls['store_or_replace'], 0) == [1, 2]
}

fn test_param_storage_sources_use_call_site_module_for_unqualified_calls() {
	dir := os.join_path(os.vtmp_dir(), 'v3_storage_source_module_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	other_path := os.join_path(dir, 'other.v')
	main_path := os.join_path(dir, 'main.v')
	os.write_file(other_path, 'module other

struct OtherBox {}

fn replace(mut target &OtherBox, replacement &OtherBox) {
	_ = target
	_ = replacement
}
') or { panic(err) }
	os.write_file(main_path, 'struct Box {}

fn replace(mut target &Box, replacement &Box) bool {
	target = replacement
	return true
}

fn main() {
	mut first := &Box{}
	second := &Box{}
	changed := replace(mut first, second)
	assert changed
}
') or { panic(err) }

	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([other_path, main_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut replace_call := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('replace') {
			replace_call = flat.NodeId(i)
		}
	}
	assert int(replace_call) >= 0
	assert tc.call_param_storage_source_params(replace_call, 0) == [1]
}

fn test_param_storage_sources_snapshot_aliases_before_multi_assignment() {
	path := os.join_path(os.vtmp_dir(), 'v3_storage_source_multi_assign_${os.getpid()}.v')
	os.write_file(path, 'struct Value {}

struct Box {
mut:
	value &Value
}

fn swap_aliases_then_store(mut target &Box, value &Value) {
	mut local_value := Value{}
	mut local := &Box{
		value: &local_value
	}
	mut alias := target
	mut other := local
	alias, other = other, alias
	other.value = value
}

fn main() {
	mut value := Value{}
	mut target := &Box{
		value: &value
	}
	swap_aliases_then_store(mut target, &value)
}
') or { panic(err) }
	defer {
		os.rm(path) or {}
	}

	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut call_id := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('swap_aliases_then_store') {
			call_id = flat.NodeId(i)
		}
	}
	assert int(call_id) >= 0
	assert tc.call_param_storage_source_params(call_id, 0) == [1]
}

fn test_param_storage_sources_follow_c_for_execution_order() {
	path := os.join_path(os.vtmp_dir(), 'v3_storage_source_c_for_${os.getpid()}.v')
	os.write_file(path, 'struct Value {}

struct Box {
mut:
	value &Value
}

fn store_in_c_for(mut target &Box, value &Value, replacement &Value) {
	mut local := &Box{
		value: unsafe { replacement }
	}
	mut alias := local
	mut i := 0
	for alias = target; i < 1; alias = local {
		alias.value = value
		i++
	}
}

fn main() {
	mut value := Value{}
	mut target := &Box{
		value: &value
	}
	store_in_c_for(mut target, &value, &value)
}
') or { panic(err) }
	defer {
		os.rm(path) or {}
	}

	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut call_id := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('store_in_c_for') {
			call_id = flat.NodeId(i)
		}
	}
	assert int(call_id) >= 0
	sources := tc.call_param_storage_source_params(call_id, 0)
	assert 1 in sources, sources.str()
}

fn test_param_storage_sources_do_not_treat_goto_bypassed_write_as_definite() {
	path := os.join_path(os.vtmp_dir(), 'v3_storage_source_goto_definite_${os.getpid()}.v')
	os.write_file(path, 'struct Value {}

struct Box {
mut:
	value &Value
}

fn maybe_replace(mut target &Box, replacement &Value, skip bool) {
	if skip {
		unsafe { goto done }
	}
	target.value = replacement
done:
}

fn wrapper(mut target &Box, first &Value, replacement &Value, skip bool) {
	target.value = first
	maybe_replace(mut target, replacement, skip)
}

fn main() {
	mut first := Value{}
	mut replacement := Value{}
	mut target := &Box{
		value: &first
	}
	wrapper(mut target, &first, &replacement, true)
}
') or { panic(err) }
	defer {
		os.rm(path) or {}
	}

	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut wrapper_call := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('wrapper') {
			wrapper_call = flat.NodeId(i)
		}
	}
	assert int(wrapper_call) >= 0
	assert tc.call_param_storage_source_params(wrapper_call, 0) == [1, 2]
}
