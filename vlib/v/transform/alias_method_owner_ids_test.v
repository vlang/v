module transform

import v.flat
import v.types

fn test_alias_method_index_keeps_numeric_owners_and_first_signature() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Count', flat.source_type_i64)
	tc.type_aliases.set_id('OtherCount', flat.source_type_i64)
	tc.type_aliases.set_id('Flag', flat.source_type_bool)
	tc.fn_param_types['Count.show'] = [types.Type(types.i64_)]
	tc.fn_param_types['OtherCount.show'] = [types.Type(types.i64_)]
	tc.fn_param_types['Count.other'] = [types.Type(types.Pointer{ base_type: types.Type(types.i64_) })]
	tc.fn_param_types['Flag.show'] = [types.Type(types.bool_)]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_alias_methods()
	assert t.alias_methods[flat.source_type_i64]['show'] == 'Count.show'
	assert t.alias_methods[flat.source_type_i64]['other'] == 'Count.other'
	assert t.alias_methods[flat.source_type_bool]['show'] == 'Flag.show'
	assert t.resolve_alias_receiver_method(flat.source_type_i64, 'show') or { '' } == 'Count.show'
	// An exact alias declaration keeps precedence over the normalized owner index.
	assert t.resolve_alias_receiver_method(flat.source_type_id('OtherCount'), 'show') or { '' } == 'OtherCount.show'
	// Collection retains the first signature registered for an owner/method pair.
	t.collect_alias_methods()
	assert t.alias_methods[flat.source_type_i64]['show'] == 'Count.show'
}

fn test_append_element_raw_identity_trims_only_outer_whitespace() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})
	for pad in [' ', '\n', '\t', '\v', '\f', '\r'] {
		assert t.array_append_elem_types_match(flat.source_type_id('${pad}[]&Item${pad}'),
			flat.source_type_id('[]&Item'))
		assert t.array_append_elem_types_match(flat.source_type_id(pad), u32(0))
	}
	assert transform_trimmed_type_id(flat.source_type_id('[] &Item')) != flat.source_type_id('[]&Item')
}
