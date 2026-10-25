module transform

import v.flat
import v.types

fn test_numeric_sum_resolution_preserves_fallback_order_and_spelling() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set_ids('dep.tast.Value', [flat.source_type_int])
	tc.sum_types.set_ids('right.alias.Ambiguous', [flat.source_type_int])
	tc.structs['exact.Value'] = []types.StructField{}
	mut t := Transformer{
		tc:         &tc
		cur_module: 'local'
		sum_cache:  &AliasCache{}
	}
	for name in ['Value', 'dep.tast.Value', 'local.Item', 'other.Item', 'left.alias.Ambiguous',
		'local.Box[int, string]', 'other.Box[int, string]', 'dep.Unspecialized'] {
		t.sum_types.set_ids(name, [flat.source_type_int])
	}
	cases := {
		'':                          ''
		'Value':                     'Value'
		'tast.Value':                'dep.tast.Value'
		'unknown.Value':             'Value'
		'Item':                      'local.Item'
		'exact.Value':               ''
		'alias.Ambiguous':           'alias.Ambiguous'
		'Box[ int,string ]':         'local.Box[int, string]'
		'Unspecialized[int]':        'dep.Unspecialized'
		'Missing[int]':              'Missing[int]'
		'Box[':                      'Box['
		'unknown.NoSum':             'unknown.NoSum'
		' Value ':                   ' Value '
		'&dep.tast.Value':           ''
		'?dep.tast.Value':           ''
		'!dep.tast.Value':           ''
		'[]dep.tast.Value':          ''
		'[2]dep.tast.Value':         ''
		'map[string]dep.tast.Value': ''
		'fn (int) dep.tast.Value':   ''
	}
	for input, expected in cases {
		input_id := flat.source_type_id(input)
		expected_id := flat.source_type_id(expected)
		assert t.resolve_sum_name_uncached(input_id) == expected_id, input
		for _ in 0 .. 3 {
			assert t.resolve_sum_name(input_id) == expected_id, input
		}
	}
	concrete := flat.source_type_id('local.Box[int, string]')
	assert t.resolve_sum_name(flat.source_type_id(c_name(flat.source_type_name(concrete)))) == concrete
}

fn test_numeric_sum_short_matches_and_interface_candidates_preserve_identity() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'local'
	tc.sum_types.set_ids('dep.Parcel[int]', [flat.source_type_int])
	tc.interface_names['Readable'] = true
	tc.interface_names['main.Readable'] = true
	tc.interface_names['local.Readable'] = true
	mut t := Transformer{ tc: &tc }
	t.sum_types.set_ids('dep.Parcel[int]', [flat.source_type_int])
	short_id := flat.source_type_id('Parcel[int]')
	concrete_id := flat.source_type_id('dep.Parcel[int]')
	assert t.find_sum_type_with_short_name(short_id) == concrete_id
	assert t.resolve_sum_name(short_id) == concrete_id
	tc.sum_types.set_ids('other.Parcel[int]', [flat.source_type_int])
	assert t.find_sum_type_with_short_name(short_id) == 0
	// The general resolver still tries the transformer's own short-name fallback first.
	assert t.resolve_sum_name(short_id) == concrete_id
	assert t.interface_impl_type_id_iface_candidates(flat.source_type_id('main.Readable')) == [
		flat.source_type_id('Readable'),
		flat.source_type_id('main.Readable'),
	]
	assert t.interface_impl_type_id_iface_candidates(flat.source_type_id('Readable')) == [
		flat.source_type_id('Readable'),
		flat.source_type_id('local.Readable'),
	]
	assert t.interface_impl_type_id_iface_candidates(0).len == 0
}
