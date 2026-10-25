module types

import v.flat

fn test_type_expression_source_operands_keep_independent_annotations() {
	mut a := flat.FlatAst.new()
	name_id := flat.source_type_id('Widget')
	name := a.add_node(flat.Node{
		kind:         .ident
		source_value: name_id
		typ:          flat.source_type_int
	})
	a.children << name
	pointer := a.add_node(flat.Node{
		kind:           .prefix
		op:             .amp
		children_count: 1
	})
	array := a.add_node(flat.Node{
		kind:         .array_init
		source_value: name_id
		typ:          flat.source_type_id('[]string')
	})
	base := a.add_node(flat.Node{ kind: .ident, source_value: flat.source_type_id('Box') })
	index_start := a.children.len
	a.children << [base, name]
	application := a.add_node(flat.Node{
		kind:           .index
		children_start: i32(index_start)
		children_count: 2
	})
	tc := TypeChecker.new(&a)
	assert tc.type_expr_name(name) == name_id
	assert tc.generic_call_type_arg_name(name) == name_id
	assert tc.type_expr_name(pointer) == flat.source_type_id('&Widget')
	assert tc.type_expr_name(array) == flat.source_type_id('[]Widget')
	assert tc.type_expr_name(application) == flat.source_type_id('Box[Widget]')
	assert tc.generic_call_type_arg_name(application) == flat.source_type_id('Box[Widget]')
}

fn test_const_cast_source_operand_is_independent_of_its_annotation() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{ kind: .int_literal, value: '42' })
	a.children << literal
	integer_cast := a.add_node(flat.Node{
		kind:           .cast_expr
		source_value:   flat.source_type_int
		typ:            flat.source_type_f64
		children_count: 1
	})
	float_cast := a.add_node(flat.Node{
		kind:           .cast_expr
		source_value:   flat.source_type_f64
		typ:            flat.source_type_int
		children_count: 1
	})
	tc := TypeChecker.new(&a)
	value := tc.const_int_expr(integer_cast, '', []string{}) or { -1 }
	assert value == 42
	assert tc.const_int_expr(float_cast, '', []string{}) == none
}

fn test_match_type_pattern_reads_numeric_source_operand() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	pattern_id := flat.source_type_id('?int')
	pattern := flat.Node{
		kind:         .ident
		source_value: pattern_id
		typ:          flat.source_type_int
	}
	matched := tc.match_type_pattern(&pattern) or { u32(0) }
	assert matched == pattern_id
}

fn test_check_ident_accepts_source_backed_and_legacy_spellings() {
	mut a := flat.FlatAst.new()
	legacy := a.add_node(flat.Node{ kind: .ident, value: 'bound' })
	canonical := a.add_node(flat.Node{
		kind:         .ident
		source_value: flat.source_type_id('bound')
	})
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.cur_scope.insert('bound', Type(int_))
	tc.check_ident(legacy, a.nodes[int(legacy)])
	tc.check_ident(canonical, a.nodes[int(canonical)])
	assert tc.expr_key(legacy) == 'bound'
	assert tc.expr_key(canonical) == 'bound'
	assert tc.expr_key_composite(canonical) == 'bound'
	assert tc.expr_key_part(canonical) == 'bound'
	assert tc.expr_key_matches(canonical, 'bound')
	assert !tc.expr_key_matches(canonical, 'other')
	assert tc.errors.len == 0
	assert tc.expr_type(legacy)? == Type(int_)
	assert tc.expr_type(canonical)? == Type(int_)
}

fn test_call_generic_argument_queries_use_numeric_payload() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{ kind: .ident, value: 'known' })
	a.children << callee
	mut fn_node := flat.Node{ kind: .fn_decl }
	fn_node.set_generic_params(['T'])
	fn_id := a.add_node(fn_node)
	mut call := flat.Node{ kind: .call, children_count: 1 }
	call.set_call_type_args([flat.source_type_id(' T ')])
	mut tc := TypeChecker.new(&a)
	tc.fn_context.node_id = int(fn_id)
	tc.fn_ret_types['known'] = Type(int_)
	assert tc.call_has_explicit_generic_args(call)
	assert tc.call_generic_args_have_placeholders(call)
	call.set_call_type_args([flat.source_type_int])
	assert tc.call_has_explicit_generic_args(call)
	assert !tc.call_generic_args_have_placeholders(call)
	call.set_call_type_args([]u32{})
	assert !tc.call_has_explicit_generic_args(call)
	assert !tc.call_generic_args_have_placeholders(call)
}

fn test_generic_call_rejects_source_backed_option_and_result_type_names() {
	for spelling in ['?int', '!int'] {
		mut a := flat.FlatAst.new()
		base := a.add_node(flat.Node{ kind: .ident, value: 'known' })
		type_arg := a.add_node(flat.Node{
			kind:         .ident
			source_value: flat.source_type_id(spelling)
			typ:          flat.source_type_f64
		})
		a.children << [base, type_arg]
		callee := a.add_node(flat.Node{ kind: .index, children_count: 2 })
		call_start := a.children.len
		a.children << callee
		call_id := a.add_node(flat.Node{
			kind:           .call
			children_start: i32(call_start)
			children_count: 1
		})
		mut tc := TypeChecker.new(&a)
		tc.reset_node_caches(a.nodes.len)
		tc.fn_ret_types['known'] = Type(int_)
		tc.check_call(call_id, a.nodes[int(call_id)])
		assert tc.errors.len == 1, tc.errors.str()
		assert tc.errors[0].kind == .call_arg_mismatch
		assert tc.errors[0].msg == 'cannot use Option type name as concrete type'
		assert tc.errors[0].node == type_arg
	}
}
