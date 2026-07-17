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

fn test_empty_interface_impl_names_include_enums() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.enum_names['Color'] = true

	impls := tc.interface_impl_names('Any')
	assert 'Color' in impls
	ids := tc.interface_type_ids('Any')
	assert ids['Color'] > 0
}

fn test_stable_interface_type_ids_resolve_hash_collisions() {
	ids := stable_interface_type_ids(['main.TZjXQlDs6', 'main.T2nAMbYQH'])
	assert ids['main.TZjXQlDs6'] != ids['main.T2nAMbYQH']
}

fn test_stable_interface_type_ids_preserve_existing_ids_after_late_collisions() {
	assert stable_interface_type_id_hash('Tnndxrxb') == stable_interface_type_id_hash('Twvlzleh')
	before := stable_interface_type_ids(['Twvlzleh'])
	after := stable_interface_type_ids(['Twvlzleh', 'Tnndxrxb'])
	assert after['Twvlzleh'] == before['Twvlzleh']
	assert after['Tnndxrxb'] != before['Twvlzleh']
}
