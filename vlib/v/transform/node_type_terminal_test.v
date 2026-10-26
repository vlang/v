module transform

import v.flat
import v.types

fn test_terminal_node_types_ignore_annotations_and_checker_guesses() {
	mut a := flat.FlatAst.new()
	kinds := [flat.NodeKind.bool_literal, .float_literal, .int_literal, .string_literal,
		.string_interp, .nil_literal, .char_literal, .sizeof_expr, .offsetof_expr]
	expected := [flat.source_type_bool, flat.source_type_f64, flat.source_type_int,
		flat.source_type_string, flat.source_type_string, flat.source_type_voidptr,
		flat.source_type_rune, flat.source_type_u32, flat.source_type_u32]
	for kind in kinds {
		a.add_node(flat.Node{ kind: kind, value: 'value', typ: flat.source_type_id('Guess') })
	}
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Guess'] = []types.StructField{}
	for i in 0 .. kinds.len {
		tc.register_synth_type(flat.NodeId(i), types.Type(types.Struct{ name_id: flat.source_type_id('Guess') }))
	}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.begin_node_type_memo(0, a.nodes.len - 1)
	for building_v in [false, true] {
		t.building_v = building_v
		for skip_generics in [false, true] {
			t.skip_generics = skip_generics
			for i, result in expected {
				id := flat.NodeId(i)
				assert t.node_type_uncached(id) == result, '${kinds[i]}'
				assert t.node_type(id) == result, '${kinds[i]}'
			}
		}
	}
	assert t.node_type(flat.NodeId(-1)) == 0
}

fn test_terminal_queries_preserve_contextual_resolution_and_rewrites() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	character := a.add_val(.char_literal, 'c:hello')
	optional := a.add_node(flat.Node{ kind: .none_expr, typ: flat.source_type_id('?string') })
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('one.Item', flat.source_type_int)
	tc.type_aliases.set_id('two.Item', flat.source_type_bool)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'one'
	t.set_var_type('value', flat.source_type_id('Item'))
	t.begin_node_type_memo(0, a.nodes.len - 1)
	assert t.node_type(value) == flat.source_type_int
	assert t.node_type(character) == flat.source_type_id('&u8')
	t.cur_module = 'two'
	t.expected_expr_type = flat.source_type_string
	t.in_const_init = true
	assert t.node_type(character) == flat.source_type_id('&u8')
	// A terminal query can defer the memo context check; the next contextual
	// query must still observe the new module and resolve its alias again.
	assert t.node_type(value) == flat.source_type_bool
	t.set_node_value(int(character), 'hello')
	assert t.node_type(character) == flat.source_type_rune
	// `none` retains the annotation fallback for its unresolved `?void` type.
	assert t.node_type(optional) == flat.source_type_id('?string')
	a.nodes[int(character)].kind = .ident
	t.set_node_value(int(character), 'value')
	assert t.node_type(character) == flat.source_type_bool
}

fn test_boolean_operator_node_types_preserve_operand_count_guards() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.ident, 'left')
	right := a.add_val(.ident, 'right')
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for op in [flat.Op.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
		start := a.begin_children()
		a.add_child(left)
		a.add_child(right)
		id := a.add_node(flat.Node{
			kind:           .infix
			op:             op
			typ:            flat.source_type_f64
			children_start: i32(start)
			children_count: 2
		})
		assert t.node_type_uncached(id) == flat.source_type_bool
		assert t.node_type(id) == flat.source_type_bool
		// Incomplete expressions retain the annotation fallback.
		a.nodes[int(id)].children_count = 1
		assert t.node_type(id) == flat.source_type_f64
	}
	start := a.begin_children()
	a.add_child(left)
	negated := a.add_node(flat.Node{
		kind:           .prefix
		op:             .not
		typ:            flat.source_type_f64
		children_start: i32(start)
		children_count: 1
	})
	assert t.node_type_uncached(negated) == flat.source_type_bool
	assert t.node_type(negated) == flat.source_type_bool
	a.nodes[int(negated)].children_count = 0
	assert t.node_type(negated) == flat.source_type_f64
}
