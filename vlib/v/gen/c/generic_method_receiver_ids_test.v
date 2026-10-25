module c

import v.flat
import v.types

fn generic_method_receiver_names_for_test(ids []u32) []string {
	mut names := []string{cap: ids.len}
	for id in ids {
		names << flat.source_type_name(id)
	}
	return names
}

fn generic_method_index_names_for_test(g &FlatGen, receiver_id u32, method string) []string {
	candidates := g.generic_method_candidates[receiver_id][method] or { return []string{} }
	mut names := []string{cap: candidates.len}
	for candidate in candidates {
		names << candidate.name
	}
	return names
}

fn test_generic_method_numeric_index_keeps_owner_method_and_append_order() {
	mut g := FlatGen.new()
	for owner in ['one.Box', 'two.Box', 'one.Box ', ' one.Box'] {
		owner_id := flat.source_type_id(owner)
		for name in ['${owner}[int].run', '${owner}[string].run'] {
			g.add_generic_method_candidate(owner_id, 'run',
				generic_method_candidate_for_decl(name, types.Type(types.int_), []types.Type{}))
		}
		g.add_generic_method_candidate(owner_id, 'other',
			generic_method_candidate_for_decl('${owner}[int].other', types.Type(types.int_),
				[]types.Type{}))
	}
	assert g.generic_method_candidates.len == 4
	for owner in ['one.Box', 'two.Box', 'one.Box ', ' one.Box'] {
		owner_id := flat.source_type_id(owner)
		assert generic_method_index_names_for_test(&g, owner_id, 'run') == [
			'${owner}[int].run',
			'${owner}[string].run',
		]
		assert generic_method_index_names_for_test(&g, owner_id, 'other') == [
			'${owner}[int].other',
		]
	}
	assert generic_method_index_names_for_test(&g, flat.source_type_id('Box'), 'run').len == 0
	assert generic_method_index_names_for_test(&g, flat.source_type_id('one.Box'), 'absent').len == 0
}

fn test_generic_method_numeric_receiver_alternatives_preserve_raw_bytes() {
	query := generic_method_receiver_query(flat.source_type_id('mod.Box[other.Type]'))
	// The legacy short spelling uses the last dot even when it occurs in an argument.
	assert generic_method_receiver_names_for_test(query.lookup_receiver_ids) == [
		'mod.Box[other.Type]',
		'Type]',
		'mod.Box',
		'Box',
	]
	assert generic_method_receiver_names_for_test(generic_method_receiver_query(flat.source_type_id('Box[int]')).lookup_receiver_ids) == [
		'Box[int]',
		'Box',
	]
	assert generic_method_receiver_names_for_test(generic_method_receiver_query(flat.source_type_id(' mod.Box [int] ')).lookup_receiver_ids) == [
		' mod.Box [int] ',
		'Box [int] ',
		' mod.Box ',
		'Box ',
	]
	assert generic_method_receiver_query(0).lookup_receiver_ids.len == 0
}

fn test_generic_method_numeric_receiver_preference_preserves_raw_qualification() {
	params := []types.Type{}
	qualified := generic_method_candidate_for_decl('mod.Box[int].run', types.Type(types.int_),
		params)
	short_generic := generic_method_candidate_for_decl('Box[int].run', types.Type(types.int_),
		params)
	short_plain := generic_method_candidate_for_decl('Box.run', types.Type(types.int_), params)
	assert generic_method_candidate_receiver_preference(qualified,
		generic_method_receiver_query(flat.source_type_id('mod.Box[int]'))) == 30
	assert generic_method_candidate_receiver_preference(qualified,
		generic_method_receiver_query(flat.source_type_id('mod.Box[string]'))) == 25
	assert generic_method_candidate_receiver_preference(short_generic,
		generic_method_receiver_query(flat.source_type_id('mod.Box[string]'))) == 15
	assert generic_method_candidate_receiver_preference(short_plain,
		generic_method_receiver_query(flat.source_type_id('mod.Box'))) == 10
	assert generic_method_candidate_receiver_preference(qualified,
		generic_method_receiver_query(flat.source_type_id('other.Box[string]'))) == 0
	assert generic_method_candidate_receiver_preference(qualified,
		generic_method_receiver_query(flat.source_type_id('mod.Box [int]'))) == 0
}

fn test_generic_method_numeric_precompute_keeps_qualified_short_and_empty_buckets() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.fn_ret_types['z.Box[int].run'] = types.Type(types.int_)
	tc.fn_ret_types['a.Box[int].run'] = types.Type(types.int_)
	mut expected := []string{}
	for name, _ in tc.fn_ret_types {
		expected << name
	}
	g.precompute_generic_method_candidate_index()
	assert generic_method_index_names_for_test(&g, flat.source_type_id('Box'), 'run') == expected
	assert generic_method_index_names_for_test(&g, flat.source_type_id('z.Box'), 'run') == [
		'z.Box[int].run',
	]
	assert generic_method_index_names_for_test(&g, flat.source_type_id('a.Box'), 'run') == [
		'a.Box[int].run',
	]
	// The raw precompute slice includes this trailing dot and its empty short name.
	tc.fn_ret_types.clear()
	tc.fn_ret_types['jsonrpc.[]Response.run'] = types.Type(types.int_)
	g.precompute_generic_method_candidate_index()
	assert generic_method_index_names_for_test(&g, flat.source_type_id('jsonrpc.'), 'run') == [
		'jsonrpc.[]Response.run',
	]
	assert generic_method_index_names_for_test(&g, 0, 'run') == ['jsonrpc.[]Response.run']
	assert generic_method_index_names_for_test(&g, flat.source_type_id('Box'), 'run').len == 0
}

fn test_generic_method_numeric_candidates_keep_duplicate_and_ambiguous_behavior() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.const_types['number'] = types.Type(types.int_)
	callee := a.add_val(.ident, 'dep.Box.run')
	arg := a.add_val(.ident, 'number')
	start := a.begin_children()
	a.add_child(callee)
	a.add_child(arg)
	call := flat.Node{ kind: .call, children_start: start, children_count: 2 }
	params := [types.Type(types.string_), types.Type(types.int_)]
	candidate := generic_method_candidate_for_decl('dep.Box[int].run', types.Type(types.int_),
		params)
	for owner in ['dep.Box', 'Box'] {
		g.add_generic_method_candidate(flat.source_type_id(owner), 'run', candidate)
	}
	// Seeing one declaration through both owner alternatives does not make it ambiguous.
	assert g.specialized_generic_method_name_for_call_args(call, 'dep.Box.run', 2)? == candidate.name
	g.add_generic_method_candidate(flat.source_type_id('Box'), 'run',
		generic_method_candidate_for_decl('dep.Box[ int ].run', types.Type(types.int_), params))
	// Equal scores and preference for two distinct declaration keys remain ambiguous.
	assert g.specialized_generic_method_name_for_call_args(call, 'dep.Box.run', 2) == none
}

fn test_generic_method_numeric_index_preserves_return_only_array_guard() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.const_types['number'] = types.Type(types.int_)
	callee := a.add_val(.ident, 'method')
	arg := a.add_val(.ident, 'number')
	start := a.begin_children()
	a.add_child(callee)
	a.add_child(arg)
	call := flat.Node{ kind: .call, children_start: start, children_count: 2 }
	params := [types.Type(types.string_), types.Type(types.int_)]
	for receiver in ['[]Response', 'jsonrpc.[]Response', '[2]Response', 'jsonrpc.[2]Response'] {
		name := '${receiver}.run'
		g.add_generic_method_candidate(flat.source_type_id(receiver), 'run',
			generic_method_candidate_for_decl(name, types.Type(types.int_), params))
		// Only the return-based fallback has the bracket guard. Keep the argument-based
		// fallback's existing search rather than adding a new guard during key migration.
		assert g.specialized_generic_method_name_for_call_with_arg_count(flat.empty_node, name, 2) == none
		assert g.specialized_generic_method_name_for_call_args(call, name, 2)? == name
	}
}
