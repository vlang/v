module markused

import v3.flat
import v3.types

fn call_helper_node(mut a flat.FlatAst, node flat.Node, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		...node
		children_start: start
		children_count: children.len
	})
}

fn test_join_path_helper_preserves_resolved_and_source_names() {
	mut a := flat.FlatAst.new()
	arg := a.add_node(flat.Node{ kind: .string_literal, value: 'part' })
	spread := call_helper_node(mut a, flat.Node{ kind: .prefix, value: '...' }, [arg])
	for base_name in ['', 'os', 'other'] {
		base := a.add_node(flat.Node{ kind: .ident, value: base_name })
		callee := if base_name == '' {
			a.add_node(flat.Node{ kind: .ident, value: 'join_path' })
		} else {
			call_helper_node(mut a, flat.Node{ kind: .selector, value: 'join_path' }, [
				base,
			])
		}
		for resolved in ['', 'os.join_path'] {
			for last_arg in [arg, spread] {
				id := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, arg, last_arg])
				c := CallCollector{ a: &a }
				mut calls := []string{}
				c.collect_lowered_join_path_single(a.node(id), resolved, mut calls)
				if last_arg == arg && (base_name != 'other' || resolved != '') {
					assert calls == ['join_path_single', 'os.join_path_single']
				} else {
					assert calls.len == 0
				}
			}
		}
	}
}

fn test_omitted_parameter_defaults_skip_receiver_and_named_arguments() {
	mut a := flat.FlatAst.new()
	default_ident := a.add_node(flat.Node{ kind: .ident, value: 'default_level' })
	default_call := call_helper_node(mut a, flat.Node{ kind: .call }, [default_ident])
	field := call_helper_node(mut a, flat.Node{ kind: .field_decl, value: 'level', typ: 'int' }, [
		default_call,
	])
	config := call_helper_node(mut a, flat.Node{ kind: .struct_decl, value: 'Config' }, [
		field,
	])
	receiver := a.add_node(flat.Node{ kind: .param, value: 'self', typ: 'Receiver', op: .dot })
	data := a.add_node(flat.Node{ kind: .param, value: 'data', typ: 'int' })
	options := a.add_node(flat.Node{ kind: .param, value: 'options', typ: 'Config' })
	decl := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'consume' }, [
		receiver,
		data,
		options,
	])
	callee := a.add_node(flat.Node{ kind: .ident, value: 'consume' })
	arg := a.add_node(flat.Node{ kind: .int_literal, value: '1' })
	named := call_helper_node(mut a, flat.Node{ kind: .field_init, value: 'level' }, [
		arg,
	])
	mut tc := types.TypeChecker.new(&a)
	tc.fn_ret_types['default_level'] = types.Type(types.int_)
	c := CallCollector{
		a: &a
		tc: &tc
		fn_decls: {
			'consume': FnDeclInfo{ node_id: decl }
		}
		struct_decls: {
			'Config': StructDeclInfo{ node_id: config }
		}
		import_contexts: [map[string]string{}]
	}
	for last_arg in [named, arg] {
		id := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, arg, last_arg])
		mut calls := []string{}
		c.collect_omitted_params_default_calls(a.node(id), 'consume', '', map[string]string{}, mut calls)
		assert ('default_level' in calls) == (last_arg == named)
	}
}

fn test_call_search_distinguishes_leaf_index_and_nested_call() {
	mut a := flat.FlatAst.new()
	ident := a.add_node(flat.Node{ kind: .ident, value: 'data' })
	index := call_helper_node(mut a, flat.Node{ kind: .index }, [ident, ident])
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [ident])
	nested := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'field' }, [
		call,
	])
	c := CallCollector{ a: &a }
	assert !c.expr_contains_call_or_index(ident)
	assert !c.expr_contains_call(ident)
	assert c.expr_contains_call_or_index(index)
	assert !c.expr_contains_call(index)
	assert c.expr_contains_call_or_index(nested)
	assert c.expr_contains_call(nested)
}
