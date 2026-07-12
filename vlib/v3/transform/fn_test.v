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
