module transform

import v.flat
import v.types

fn test_node_type_memo_follows_binding_changes_and_rewritten_descendants() {
	mut a := flat.FlatAst.new()
	values := a.add_node(flat.Node{ kind: .ident, value: 'values' })
	index := a.add_node(flat.Node{ kind: .int_literal, value: '0' })
	start := a.begin_children()
	a.add_child(values)
	a.add_child(index)
	lookup := a.add_node(flat.Node{
		kind:           .index
		children_start: i32(start)
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	configure_transformer(mut t, false, false, false, false, false, unsafe { nil })
	assert t.memo_node_types
	t.set_var_type('values', flat.source_type_id('[]int'))
	t.set_var_type('other', flat.source_type_id('[]bool'))
	t.begin_node_type_memo(0, a.nodes.len - 1)
	assert t.node_type(lookup) == flat.source_type_int
	assert t.node_type(lookup) == flat.source_type_int
	t.set_var_type('values', flat.source_type_id('[]string'))
	assert t.node_type(lookup) == flat.source_type_string
	// Only the leaf changes. Its cached index ancestor must be resolved again.
	t.set_node_value(int(values), 'other')
	assert t.node_type(lookup) == flat.source_type_bool
	t.set_node_value(int(values), 'values')
	assert t.node_type(lookup) == flat.source_type_string
	saved := t.var_types.clone()
	t.set_var_type('values', flat.source_type_id('[]u8'))
	assert t.node_type(lookup) == flat.source_type_u8
	t.restore_var_types(saved)
	assert t.node_type(lookup) == flat.source_type_string
}

fn test_node_type_memo_follows_smartcasts_and_module_context() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{ kind: .ident, value: 'value' })
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set(flat.source_type_id('Collection'), ['[]int', 'map[string]bool'])
	tc.type_aliases.set_id(flat.source_type_id('one.Item'), flat.source_type_int)
	tc.type_aliases.set_id(flat.source_type_id('two.Item'), flat.source_type_bool)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types.set_ids(flat.source_type_id('Collection'), tc.sum_types.type_ids(flat.source_type_id('Collection')) or { []u32{} })
	t.set_var_type('value', flat.source_type_id('Collection'))
	t.begin_node_type_memo(0, a.nodes.len - 1)
	assert flat.source_type_name(t.node_type(value)) == 'Collection'
	for variant in ['[]int', 'map[string]bool'] {
		t.push_smartcast('value', flat.source_type_id(variant), flat.source_type_id('Collection'))
		assert flat.source_type_name(t.node_type(value)) == variant
		t.pop_smartcast()
		assert flat.source_type_name(t.node_type(value)) == 'Collection'
	}
	t.cur_module = 'one'
	t.set_var_type('value', flat.source_type_id('Item'))
	assert t.node_type(value) == flat.source_type_int
	t.cur_module = 'two'
	assert t.node_type(value) == flat.source_type_bool
	t.cur_module = 'one'
	assert t.node_type(value) == flat.source_type_int
}

fn test_node_type_memo_epoch_wrap_does_not_restore_old_results() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{ kind: .ident, value: 'value' })
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('value', flat.source_type_int)
	t.begin_node_type_memo(0, 0)
	assert t.node_type(value) == flat.source_type_int
	assert t.node_type_memo.filled[0] == 1
	t.node_type_memo.generation = u32(0xffffffff)
	t.clear_node_type_memo()
	assert t.node_type_memo.generation == 1
	assert t.node_type_memo.filled[0] == 0
	t.set_var_type('value', flat.source_type_bool)
	assert t.node_type(value) == flat.source_type_bool
}

fn test_binding_setter_indexes_hand_built_bindings_and_keeps_undo() {
	mut t := Transformer{
		var_types: [VarTypeBinding{ name: 'value', typ: flat.source_type_int }]
	}
	t.set_var_type('value', flat.source_type_string)
	assert t.var_type_indices['value'] == 0
	t.var_type_undo_active = true
	mark := t.mark_var_type_scope()
	t.set_var_type('value', flat.source_type_bool)
	t.set_var_type('inner', flat.source_type_u8)
	assert t.var_types[t.var_type_index('value')].typ == flat.source_type_bool
	t.restore_var_type_scope(mark)
	assert t.var_type_indices.len == 1
	assert t.var_types[t.var_type_index('value')].typ == flat.source_type_string
	assert t.var_type_index('inner') == -1
}

fn test_struct_guess_comparison_follows_checked_ids_and_call_annotation_precedence() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	call := a.add_node(flat.Node{ kind: .call, typ: flat.source_type_id('Guess') })
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('Guess')] = []types.StructField{}
	tc.register_synth_type(value, types.Type(types.Pointer{ base_type: types.Type(types.bool_) }))
	tc.register_synth_type(call, types.Type(types.void_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for _ in 0 .. 2 {
		assert flat.source_type_name(t.checker_type_over_struct_guess(value, flat.source_type_id('Guess')) or { panic('missing override') }) ==
			'&bool'
		// The checked call entry is void; its annotation supplies the usable type.
		assert t.checker_type_over_struct_guess(call, flat.source_type_id('Guess')) == none
	}
	tc.register_synth_type(value, types.Type(types.Struct{ name_id: flat.source_type_id('Guess') }))
	assert t.checker_type_over_struct_guess(value, flat.source_type_id('Guess')) == none
}

fn test_raw_checker_node_type_reference_preserves_cache_and_call_precedence() {
	for sparse in [false, true] {
		mut a := flat.FlatAst.new()
		value := a.add_val(.ident, 'values')
		void_value := a.add_val(.sql_expr, 'void_value')
		fallback := a.add_val(.int_literal, '42')
		call := a.add_node(flat.Node{ kind: .call, typ: flat.source_type_id('[]string') })
		start := a.begin_children()
		a.add_child(value)
		decl := a.add_node(flat.Node{
			kind:           .decl_assign
			children_start: i32(start)
			children_count: 1
		})
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = sparse
		tc.register_synth_type(value, types.Type(types.Array{ elem_type: types.Type(types.bool_) }))
		tc.register_synth_type(void_value, types.Type(types.void_))
		tc.register_synth_type(call, types.Type(types.void_))
		t := new_transformer(mut a, &tc, map[string]bool{})
		for _ in 0 .. 2 {
			assert flat.source_type_name(t.raw_checker_node_type(value)) == '[]bool'
			assert flat.source_type_name(t.checker_expr_type_name(value) or { panic('missing array type') }) == '[]bool'
			assert flat.source_type_name(t.infer_decl_type(a.node(decl))) == '[]bool'
			assert t.raw_checker_node_type(void_value) == 0
			assert t.checker_expr_type_name(void_value) == none
			// The call annotation supplies the result when Void is not cached.
			assert flat.source_type_name(t.raw_checker_node_type(call)) == '[]string'
			assert flat.source_type_name(t.checker_expr_type_name(call) or { panic('missing call type') }) == '[]string'
			// No checked value is available for this literal.
			assert t.raw_checker_node_type(fallback) == flat.source_type_int
			assert (t.checker_expr_type_name(fallback) or { panic('missing literal type') }) == flat.source_type_int
			assert t.raw_checker_node_type(flat.NodeId(-1)) == 0
		}
		tc.register_synth_type(value, types.Type(types.Array{ elem_type: types.Type(types.u8_) }))
		assert flat.source_type_name(t.raw_checker_node_type(value)) == '[]u8'
		assert flat.source_type_name(t.checker_expr_type_name(value) or { panic('missing updated array type') }) == '[]u8'
		assert flat.source_type_name(t.infer_decl_type(a.node(decl))) == '[]u8'
	}
	no_checker := Transformer{}
	assert no_checker.raw_checker_node_type(flat.NodeId(0)) == 0
	assert no_checker.checker_expr_type_name(flat.NodeId(0)) == none
}
