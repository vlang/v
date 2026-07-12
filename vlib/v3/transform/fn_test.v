module transform

import v3.flat

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
}
