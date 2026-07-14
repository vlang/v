module transform

import v3.flat
import v3.types

fn test_flattened_generic_receiver_short_variants() {
	assert flattened_generic_receiver_short_variants('foo__Bar_baz__Qux') == [
		'Bar_Qux',
	]
	assert flattened_generic_receiver_short_variants('mod.foo__Bar_baz__Qux') == [
		'Bar_Qux',
		'mod.Bar_Qux',
	]
}

fn test_generic_inference_uses_seeded_mut_param_value_type_while_cloning() {
	mut a := flat.FlatAst.new()
	ident_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'value'
		typ:   '&Concrete'
	})
	mut t := Transformer{
		a:                        &a
		in_monomorphize_scan:     true
		cloning_generic_fn_depth: 1
		var_types:                [
			VarTypeBinding{
				name:    'value'
				typ:     'Concrete'
				raw_typ: 'Concrete'
			},
		]
		mut_param_values:         {
			'value': true
		}
	}
	assert t.generic_call_arg_type_for_inference(ident_id) == 'Concrete'
}

fn test_typeof_display_canonicalizes_fixed_array_map_values() {
	assert typeof_display_type_text('map[string]int[3]') == 'map[string][3]int'
	assert typeof_display_type_text('int[n]') == '[n]int'
	assert typeof_display_type_text('map[string]int[config.size]') == 'map[string][config.size]int'
	assert typeof_display_type_text('int[n + 1]') == '[n + 1]int'
	assert typeof_display_type_text('int[0x10]') == '[0x10]int'
	assert typeof_display_type_text('Box[T]') == 'Box[T]'
	assert typeof_display_type_text('Box[int]') == 'Box[int]'
	assert typeof_display_type_text('Box[types.Node]') == 'Box[types.Node]'
	assert typeof_display_type_text('Box[fn () int]') == 'Box[fn () int]'
	assert typeof_display_type_text('Box[chan int]') == 'Box[chan int]'
	assert typeof_display_type_text('chan int[3]') == 'chan [3]int'
	assert typeof_display_type_text('Box[int[3]]') == 'Box[[3]int]'
	assert typeof_display_type_text('Pair[int[3], Box[string[2]]]') == 'Pair[[3]int, Box[[2]string]]'
	assert typeof_display_type_text('Box[int][3]') == '[3]Box[int]'
	fixed_maps := types.Type(types.ArrayFixed{
		elem_type: types.Type(types.Map{
			key_type:   types.Type(types.String{})
			value_type: types.Type(types.int_)
		})
		len:       3
	})
	assert typeof_display_resolved_type_text(fixed_maps) == '[3]map[string]int'
}

fn test_parallel_worker_reuses_prebuilt_call_param_decl_index() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'signature_index_test.v')
	a.add_val(.module_decl, 'main')
	param_id := a.add_node(flat.Node{
		kind:  .param
		value: 'value'
		typ:   'string'
	})
	children_start := a.children.len
	a.children << param_id
	a.add_node(flat.Node{
		kind:           .fn_decl
		value:          'takes_string'
		children_start: children_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare_parallel_call_param_types()
	mut worker := t.fork_worker(t.a, t.tc)
	assert worker.call_param_types_index_ready
	assert worker.call_param_types_decl_index.len == t.call_param_types_decl_index.len
	assert worker.call_param_types_decl_cache.len == t.call_param_types_decl_cache.len
	params := worker.call_param_types_from_decl('takes_string') or {
		assert false
		return
	}
	assert params.len == 1
	assert params[0] is types.String
}

fn test_multi_return_selector_suffix_does_not_match_free_fn() {
	mut a := flat.FlatAst.new()
	receiver_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'value'
	})
	selector_children_start := a.children.len
	a.children << receiver_id
	selector_id := a.add_node(flat.Node{
		kind:           .selector
		value:          'pair'
		children_start: i32(selector_children_start)
		children_count: 1
	})
	call_children_start := a.children.len
	a.children << selector_id
	call_id := a.add_node(flat.Node{
		kind:           .call
		children_start: i32(call_children_start)
		children_count: 1
	})
	multi_return := types.Type(types.MultiReturn{
		types: [types.Type(types.int_), types.Type(types.string_)]
	})
	mut tc := types.TypeChecker.new(a)
	tc.fn_ret_types['pair'] = multi_return
	mut t := Transformer{
		a:                            &a
		tc:                           &tc
		receiver_method_suffix_index: {
			'pair': 'pair'
		}
	}
	call := a.nodes[int(call_id)]
	if _ := t.find_multi_return_call_types(call, 2) {
		assert false, 'selector suffix lookup matched the free pair function'
	}

	tc.fn_ret_types['Container.pair'] = multi_return
	t.receiver_method_suffix_index['pair'] = 'Container.pair'
	items := t.find_multi_return_call_types(call, 2) or {
		assert false, 'selector suffix lookup did not match the receiver method'
		return
	}
	assert items.len == 2
}
