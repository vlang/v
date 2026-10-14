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
	assert flat.source_type_name(t.node_type(lookup)) == 'int'
	assert flat.source_type_name(t.node_type(lookup)) == 'int'
	t.set_var_type('values', flat.source_type_id('[]string'))
	assert flat.source_type_name(t.node_type(lookup)) == 'string'
	// Only the leaf changes. Its cached index ancestor must be resolved again.
	t.set_node_value(int(values), 'other')
	assert flat.source_type_name(t.node_type(lookup)) == 'bool'
	t.set_node_value(int(values), 'values')
	assert flat.source_type_name(t.node_type(lookup)) == 'string'
	saved := t.var_types.clone()
	t.set_var_type('values', flat.source_type_id('[]u8'))
	assert flat.source_type_name(t.node_type(lookup)) == 'u8'
	t.restore_var_types(saved)
	assert flat.source_type_name(t.node_type(lookup)) == 'string'
}

fn test_node_type_memo_follows_smartcasts_and_module_context() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{ kind: .ident, value: 'value' })
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set('Collection', ['[]int', 'map[string]bool'])
	tc.type_aliases.set('one.Item', 'int')
	tc.type_aliases.set('two.Item', 'bool')
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types.set('Collection', tc.sum_types.value('Collection'))
	t.set_var_type('value', flat.source_type_id('Collection'))
	t.begin_node_type_memo(0, a.nodes.len - 1)
	assert flat.source_type_name(t.node_type(value)) == 'Collection'
	for variant in ['[]int', 'map[string]bool'] {
		t.push_smartcast('value', variant, 'Collection')
		assert flat.source_type_name(t.node_type(value)) == variant
		t.pop_smartcast()
		assert flat.source_type_name(t.node_type(value)) == 'Collection'
	}
	t.cur_module = 'one'
	t.set_var_type('value', flat.source_type_id('Item'))
	assert flat.source_type_name(t.node_type(value)) == 'int'
	t.cur_module = 'two'
	assert flat.source_type_name(t.node_type(value)) == 'bool'
	t.cur_module = 'one'
	assert flat.source_type_name(t.node_type(value)) == 'int'
}

fn test_node_type_memo_epoch_wrap_does_not_restore_old_results() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{ kind: .ident, value: 'value' })
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('value', flat.source_type_id('int'))
	t.begin_node_type_memo(0, 0)
	assert flat.source_type_name(t.node_type(value)) == 'int'
	assert t.node_type_memo.filled[0] == 1
	t.node_type_memo.generation = u32(0xffffffff)
	t.clear_node_type_memo()
	assert t.node_type_memo.generation == 1
	assert t.node_type_memo.filled[0] == 0
	t.set_var_type('value', flat.source_type_id('bool'))
	assert flat.source_type_name(t.node_type(value)) == 'bool'
}

fn test_struct_guess_comparison_follows_checked_ids_and_call_annotation_precedence() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	call := a.add_node(flat.Node{ kind: .call, typ: flat.source_type_id('Guess') })
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Guess'] = []types.StructField{}
	tc.register_synth_type(value, types.Type(types.Pointer{ base_type: types.Type(types.bool_) }))
	tc.register_synth_type(call, types.Type(types.void_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for _ in 0 .. 2 {
		assert t.checker_type_over_struct_guess(value, 'Guess') or { panic('missing override') } ==
			'&bool'
		// The checked call entry is void; its annotation supplies the usable type.
		assert t.checker_type_over_struct_guess(call, 'Guess') == none
	}
	tc.register_synth_type(value, types.Type(types.Struct{ name_id: flat.source_type_id('Guess') }))
	assert t.checker_type_over_struct_guess(value, 'Guess') == none
}
