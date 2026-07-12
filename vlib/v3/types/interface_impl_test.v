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
