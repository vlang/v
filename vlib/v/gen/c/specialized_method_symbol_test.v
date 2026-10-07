module c

import v.flat
import v.types

fn test_specialized_method_calls_use_selected_declaration_symbols() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'mcp')
	node_id := a.add_val(.fn_decl, 'Request[mcp.CompletionRequestParams].decode_params')
	a.specialized_fn_nodes[int(node_id)] = true
	a.specialized_fn_modules[int(node_id)] = 'mcp'
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'mcp'
	tc.structs['mcp.CompletionRequestParams'] = []types.StructField{}
	tc.specialized_generic_fns['mcp.Request[CompletionRequestParams].decode_params'] = true
	tc.fn_ret_types['mcp.Request[CompletionRequestParams].decode_params'] = types.Type(types.string_)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	items := g.collect_fn_gen_items()
	assert items.len == 1
	callee := 'mcp.Request[CompletionRequestParams].decode_params'
	assert g.direct_call_name(callee) == items[0].c_name
	assert g.direct_call_name_for_call(flat.empty_node, callee) == items[0].c_name
	assert g.direct_call_name('mcp.Request[mcp.CompletionRequestParams].decode_params') == items[0].c_name
}

fn test_specialized_method_symbols_preserve_same_named_argument_owners() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'mcp')
	for arg in ['first.Params', 'second.Params'] {
		id := a.add_val(.fn_decl, 'Request[${arg}].decode_params')
		a.specialized_fn_nodes[int(id)] = true
		a.specialized_fn_modules[int(id)] = 'mcp'
	}
	mut tc := types.TypeChecker.new(&a)
	tc.structs['first.Params'] = []types.StructField{}
	tc.structs['second.Params'] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	items := g.collect_fn_gen_items()
	assert items.len == 2
	for idx, module_name in ['first', 'second'] {
		tc.cur_module = module_name
		assert g.direct_call_name('mcp.Request[Params].decode_params') == items[idx].c_name
	}
}
