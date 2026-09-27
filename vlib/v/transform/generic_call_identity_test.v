module transform

import v.flat
import v.types

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
	t.structs['Context'] = StructInfo{ name: 'Context', module: 'main' }
	t.structs['callee.Context'] = StructInfo{ name: 'Context', module: 'callee' }
	t.cur_module = 'callee'
	t.cur_file = 'callee.v'
	t.ensure_node_context_map_capacity()
	t.mark_node_context(id, 'main', 'main.v')
	t.generic_specialization_args['callee.run_T_Context'] = ['Context']
	t.generic_call_spec_cache[int(id)] = GenericCallSpec{
		decl_key: 'callee.run'
		args:     ['main.Context']
	}
	key, args := t.cached_generic_call_specialization(id, call, 'main', decls) or {
		assert false, 'the missing body must retain its recorded type'
		return
	}
	assert key == 'callee.run'
	assert args == ['Context']
	t.generic_fn_spec_nodes[t.generic_specialization_progress_key(decl, args)] = id
	assert t.cached_generic_call_specialization(id, call, 'main', decls) == none
}
