module transform

import v.flat
import v.types

fn test_call_name_keeps_qualified_current_module_and_checks_foreign_names() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'run')
	start := a.begin_children()
	a.add_child(callee)
	call := flat.Node{
		kind:           .call
		children_start: start
		children_count: 1
	}
	id := a.add_node(call)
	mut tc := types.TypeChecker.new(&a)
	tc.begin_sparse_transform_node_caches(0)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'worker'
	tc.sparse_resolved_call_names[int(id)] = 'worker.run'
	assert t.call_name_for_node(id, call) == 'worker.run'
	tc.sparse_resolved_call_names[int(id)] = 'foreign.run'
	assert t.call_name_for_node(id, call) == 'foreign.run'
	t.fn_ret_types.set('worker.run', 'int')
	assert t.call_name_for_node(id, call) == 'worker.run'
	t.cur_module = 'worker_more'
	assert t.call_name_for_node(id, call) == 'foreign.run'
}

fn test_rewritten_generic_call_keeps_caller_type_in_foreign_scan() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'callee.run_T_Context')
	start := a.children.len
	a.children << callee
	call := flat.Node{
		kind:           .call
		value:          'Context'
		children_start: start
		children_count: 1
	}
	id := a.add_node(call)
	mut fn_node := flat.Node{ kind: .fn_decl, value: 'run' }
	fn_node.set_generic_params(['T'])
	decl := GenericFnDecl{ node: fn_node, module: 'callee', key: 'callee.run' }
	decls := {
		'callee.run': decl
	}
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Context'] = StructInfo{ name: flat.source_type_id('Context'), module: 'main' }
	t.structs['callee.Context'] = StructInfo{ name: flat.source_type_id('Context'), module: 'callee' }
	t.cur_module = 'callee'
	t.cur_file = 'callee.v'
	t.ensure_node_context_map_capacity()
	t.mark_node_context(id, 'main', 'main.v')
	t.generic_specialization_args.set('callee.run_T_Context', ['Context'])
	t.generic_call_spec_cache[int(id)] = GenericCallSpec{
		decl_key: 'callee.run'
		args:     flat.TypeTextList.from_names(['main.Context'])
	}
	key, args_source_ids := t.cached_generic_call_specialization(id, call, 'main', decls) or {
		assert false, 'the missing body must retain its recorded type'
		return
	}
	args := flat.TypeTextList{ ids: args_source_ids }.names()

	assert key == 'callee.run'
	assert args == ['Context']
	t.generic_fn_spec_nodes[t.generic_specialization_progress_key(decl, args_source_ids)] = id
	assert t.cached_generic_call_specialization(id, call, 'main', decls) == none
}
