module types

import v.flat

fn test_known_qualified_single_letter_types_preserve_wrappers() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'single'
	tc.structs['single.M'] = []StructField{}
	for spelling in ['!single.M', '?single.M', '![]single.M', '?&single.M', 'map[string]single.M',
		'[]single.M'] {
		typ := tc.parse_type(spelling)
		assert typ.name() == spelling, '${spelling}: ${typ.name()}'
	}
	assert tc.parse_type('!M').name() == '!single.M'
	assert tc.parse_type('?M').name() == '?single.M'
}
