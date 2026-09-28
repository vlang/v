module transform

import v.flat
import v.token
import v.types

fn test_inferred_globals_use_checked_pointer_and_array_types() {
	for type_name in ['&int', 'GlobalValues'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.type_aliases['GlobalValues'] = '[3]int'
		tc.file_scope.insert('values', tc.parse_type(type_name))
		initializer := a.add_val(.int_literal, '0')
		field_start := a.begin_children()
		a.add_child(initializer)
		field := a.add_node(flat.Node{
			kind:           .field_decl
			value:          'values'
			children_start: field_start
			children_count: 1
		})
		global_start := a.begin_children()
		a.add_child(field)
		a.add_node(flat.Node{
			kind:           .global_decl
			children_start: global_start
			children_count: 1
		})
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.collect_types()
		// Transform stores normalized aliases, while keeping pointer indirection.
		expected := if type_name == 'GlobalValues' { '[3]int' } else { '&int' }
		assert t.globals['values'] == expected
	}
}

fn test_translated_global_array_decay_keeps_evaluation_order_statements() {
	mut a := flat.FlatAst.new()
	file := '/tmp/translated_global_array_arithmetic.v'
	a.source_files[1] = token.File.unindexed(file, 1)
	pos := token.new_pos(1, 0)
	callee := a.add_val(.ident, 'offset')
	call_start := a.begin_children()
	a.add_child(callee)
	offset := a.add_node(flat.Node{
		kind:           .call
		children_start: call_start
		children_count: 1
		typ:            'int'
		pos:            pos
	})
	values := a.add_node(flat.Node{ kind: .ident, value: 'values', typ: '[3]int', pos: pos })
	infix_start := a.begin_children()
	a.add_child(offset)
	a.add_child(values)
	initializer := a.add_node(flat.Node{
		kind:           .infix
		op:             .plus
		children_start: infix_start
		children_count: 2
		typ:            '&int'
		pos:            pos
	})
	field_start := a.begin_children()
	a.add_child(initializer)
	field := a.add_node(flat.Node{
		kind:           .field_decl
		value:          'second'
		children_start: field_start
		children_count: 1
	})
	global_start := a.begin_children()
	a.add_child(field)
	global_id := a.add_node(flat.Node{
		kind:           .global_decl
		children_start: global_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.translated_files[file] = true
	tc.register_synth_type(offset, types.Type(types.int_))
	tc.register_synth_type(values, tc.parse_type('[3]int'))
	tc.register_synth_type(initializer, tc.parse_type('&int'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_file = file
	t.transform_global_decl(a.nodes[int(global_id)])
	transformed := a.child_node(a.node(field), 0)
	assert transformed.kind == .block
	assert transformed.children_count == 2
	assert a.child_node(transformed, 0).kind == .decl_assign
	assert a.child_node(transformed, 1).kind == .infix
	assert t.pending_stmts.len == 0
}
