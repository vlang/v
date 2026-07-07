module transform

import v3.flat

fn test_lambda_capture_collector_scans_callee_and_range_bound() {
	mut a := flat.FlatAst.new()
	cb_id := a.add_val(.ident, 'cb')
	n_id := a.add_val(.ident, 'n')
	call_start := a.children.len
	a.children << cb_id
	a.children << n_id
	call_id := a.add_node(flat.Node{
		kind:           .call
		children_start: call_start
		children_count: 2
	})

	key_id := a.add_val(.ident, 'i')
	low_id := a.add_val(.int_literal, '0')
	limit_id := a.add_val(.ident, 'limit')
	body_id := a.add_val(.ident, 'i')
	for_start := a.children.len
	a.children << key_id
	a.children << flat.empty_node
	a.children << low_id
	a.children << limit_id
	a.children << body_id
	for_id := a.add_node(flat.Node{
		kind:           .for_in_stmt
		children_start: for_start
		children_count: 5
		value:          '4'
	})

	block_start := a.children.len
	a.children << call_id
	a.children << for_id
	block_id := a.add_node(flat.Node{
		kind:           .block
		children_start: block_start
		children_count: 2
	})

	mut t := Transformer{
		a:         &a
		var_types: [
			VarTypeBinding{
				name: 'cb'
				typ:  'fn (int) int'
			},
			VarTypeBinding{
				name: 'limit'
				typ:  'int'
			},
		]
	}
	capture_ids := t.lambda_capture_ids(block_id, {
		'n': true
	})
	mut capture_names := []string{}
	for id in capture_ids {
		capture_names << a.nodes[int(id)].value
	}
	assert 'cb' in capture_names
	assert 'limit' in capture_names
	assert 'i' !in capture_names
	assert 'n' !in capture_names
}

fn test_decl_param_type_in_module_qualifies_fn_type_params() {
	t := Transformer{
		structs: {
			'callee.S':      StructInfo{}
			'callee.Result': StructInfo{}
		}
	}
	assert t.decl_param_type_in_module('fn(S) int', 'callee') == 'fn(callee.S) int'
	assert t.decl_param_type_in_module('fn(cb fn(S) Result) Result', 'callee') == 'fn(fn(callee.S) callee.Result) callee.Result'
}
