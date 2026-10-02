module transform

import v.flat
import v.types
import v.token

fn test_inner_pointer_declarations_shadow_and_restore_outer_storage_markers() {
	for mode in ['statement', 'expression', 'typed'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		pointer_type := types.Type(types.Pointer{ base_type: types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 }) })
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.set_var_type('pointer', '&[2]int')
		t.set_var_type('values', '&[2]int')
		t.heaped_amp_locals['values'] = true
		t.pointer_value_lvalues['values'] = true
		t.pointer_value_rvalues['values'] = true
		pointer := t.make_ident('pointer')
		tc.register_synth_type(pointer, pointer_type)
		decl := t.make_decl_assign_typed('values', pointer, '&[2]int')
		t.a.nodes[int(decl)].pos = token.new_span(1, 1, 10)
		value := t.make_ident('values')
		tc.register_synth_type(value, pointer_type)
		block := t.make_block([decl, t.make_expr_stmt(value)])
		lowered := if mode == 'statement' {
			t.transform_block_stmt(block, t.a.nodes[int(block)])[0]
		} else if mode == 'expression' {
			t.transform_block_expr(block, t.a.nodes[int(block)])
		} else {
			t.transform_block_expr_for_type(block, t.a.nodes[int(block)], '&[2]int') or {
				panic('expected a typed block')
			}
		}
		lowered_node := t.a.nodes[int(lowered)]
		tail := t.a.child_node(&lowered_node, lowered_node.children_count - 1)
		assert tail.kind == .expr_stmt
		assert t.a.child_node(tail, 0).kind == .ident
		assert t.heaped_amp_locals['values']
		assert t.pointer_value_lvalues['values']
		assert t.pointer_value_rvalues['values']
		assert t.var_type('values') == '&[2]int'
	}
}

fn test_single_shadow_declaration_reads_incoming_heap_storage_before_rebinding() {
	for target in ['copied', 'value'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		_ = t.heap_escaping_value_decl('value', 'int', 'int', t.make_int_literal(1), false)
		value := t.make_ident('value')
		tc.register_synth_type(value, types.Type(types.int_))
		start := t.a.children.len
		t.a.children << [t.make_ident(target), value]
		decl := t.a.add_node(flat.Node{
			kind:           .decl_assign
			typ:            'int'
			children_start: start
			children_count: 2
			pos:            token.new_span(1, 1, 10)
		})
		lowered := t.transform_decl_assign_stmt(decl, t.a.nodes[int(decl)])
		assert lowered.len > 0
		mut reads_incoming_storage := false
		for lowered_id in lowered {
			copied := t.a.nodes[int(lowered_id)]
			if copied.kind == .decl_assign && copied.children_count == 2 {
				rhs := t.a.child_node(&copied, 1)
				if rhs.kind == .prefix && rhs.op == .mul {
					reads_incoming_storage = t.a.child_node(rhs, 0).value == 'value'
				}
			}
		}
		assert reads_incoming_storage
		assert t.heaped_amp_locals['value'] == (target != 'value')
		assert t.pointer_value_lvalues['value'] == (target != 'value')
		assert t.pointer_value_rvalues['value'] == (target != 'value')
	}
}

fn test_multi_declarations_clear_only_lhs_markers_after_reading_rhs_bindings() {
	for target in ['copied', 'values'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		fixed_type := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.set_var_type('values', '&[2]int')
		t.heaped_amp_locals['values'] = true
		t.pointer_value_lvalues['values'] = true
		t.pointer_value_rvalues['values'] = true
		value := t.make_ident('values')
		tc.register_synth_type(value, fixed_type)
		start := t.a.children.len
		t.a.children << [t.make_ident('zero'), t.make_int_literal(0), t.make_ident(target), value]
		decl := t.a.add_node(flat.Node{
			kind:           .decl_assign
			value:          '2'
			children_start: start
			children_count: 4
			pos:            token.new_span(1, 1, 10)
		})
		lowered := t.transform_decl_assign_stmt(decl, t.a.nodes[int(decl)])
		assert lowered.len == 2
		copied := t.a.nodes[int(lowered[1])]
		rhs := t.a.child_node(&copied, 1)
		assert rhs.kind == .prefix
		assert rhs.op == .mul
		assert t.heaped_amp_locals['values'] == (target != 'values')
		assert t.pointer_value_lvalues['values'] == (target != 'values')
		assert t.pointer_value_rvalues['values'] == (target != 'values')
	}
}
