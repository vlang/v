module transform

import v.flat
import v.types

fn signature_recent_decl(mut a flat.FlatAst, name string, typ u32) flat.NodeId {
	param := a.add_node(flat.Node{ kind: .param, value: 'value', typ: typ })
	start := a.children.len
	a.children << param
	return a.add_node(flat.Node{
		kind:           .fn_decl
		value:          name
		children_start: start
		children_count: 1
	})
}

fn test_signature_recent_preserves_cold_result_storage_and_equivalent_names() {
	mut a := flat.FlatAst.new()
	signature_recent_decl(mut a, 'target', flat.source_type_string)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	name := 'target'.clone()
	mut first := t.call_param_types_from_decl(name) or { panic('missing signature') }
	first[0] = types.Type(types.bool_)
	for query in [name, name.clone(), name] {
		params := t.call_param_types_from_decl(query) or { panic('missing signature') }
		assert params.len == 1
		assert params[0] is types.String
	}
}

fn test_signature_recent_invalidates_missing_and_positive_answers_on_index_rebuild() {
	mut a := flat.FlatAst.new()
	decl := signature_recent_decl(mut a, 'target', flat.source_type_string)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	name := 'target'.clone()
	missing := 'later'.clone()
	assert t.call_param_types_from_decl(missing) == none
	params := t.call_param_types_from_decl(name) or { panic('missing signature') }
	assert params[0] is types.String
	later := signature_recent_decl(mut a, missing, flat.source_type_bool)
	t.add_call_param_types_decl_key(missing, int(later), '', '')
	late_params := t.call_param_types_from_decl(missing) or { panic('missing late signature') }
	assert late_params[0] == types.Type(types.bool_)
	a.nodes[int(decl)].value = 'renamed'
	signature_recent_decl(mut a, name, flat.source_type_int)
	t.call_param_types_index_ready = false
	new_params := t.call_param_types_from_decl(name) or { panic('missing replacement signature') }
	assert new_params[0] == types.Type(types.int_)
}

fn test_signature_recent_unresolved_declaration_is_not_a_negative_answer() {
	mut a := flat.FlatAst.new()
	decl := signature_recent_decl(mut a, 'target', flat.source_type_unknown)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	name := 'target'.clone()
	assert t.call_param_types_from_decl(name) == none
	param := a.child(&a.nodes[int(decl)], 0)
	a.nodes[int(param)].typ = flat.source_type_bool
	params := t.call_param_types_from_decl(name) or { panic('unresolved answer was retained') }
	assert params[0] == types.Type(types.bool_)
}
