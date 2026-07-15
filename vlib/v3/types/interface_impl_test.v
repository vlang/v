module types

import v3.flat

fn test_empty_interface_impl_names_deduplicate_builtin_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases['byte'] = 'u8'
	tc.type_aliases['builtin.byte'] = 'u8'

	impls := tc.interface_impl_names('Any')
	assert impls.filter(it == 'byte').len == 1
	assert 'builtin.byte' !in impls
}

fn test_stable_interface_type_ids_resolve_hash_collisions() {
	ids := stable_interface_type_ids(['main.TZjXQlDs6', 'main.T2nAMbYQH'])
	assert ids['main.TZjXQlDs6'] != ids['main.T2nAMbYQH']
}

fn test_interface_type_ids_preserve_snapshot_when_late_impl_collides() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_impl_name_snapshots['Any'] = ['Twvlzleh']
	tc.structs['Twvlzleh'] = []StructField{}
	tc.structs['Tnndxrxb'] = []StructField{}

	snapshot_ids := stable_interface_type_ids(['Twvlzleh'])
	combined_ids := tc.interface_type_ids('Any')
	assert combined_ids['Twvlzleh'] == snapshot_ids['Twvlzleh']
	assert combined_ids['Tnndxrxb'] != combined_ids['Twvlzleh']
}
