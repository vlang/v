module types

import v.flat

fn test_known_qualified_single_letter_types_preserve_wrappers() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'single'
	tc.structs[flat.source_type_id('single.M')] = []StructField{}
	for spelling in ['!single.M', '?single.M', '![]single.M', '?&single.M', 'map[string]single.M',
		'[]single.M'] {
		typ := tc.parse_type(flat.source_type_id(spelling))
		assert typ.name() == spelling, '${spelling}: ${typ.name()}'
	}
	assert tc.parse_type(flat.source_type_id('!M')).name() == '!single.M'
	assert tc.parse_type(flat.source_type_id('?M')).name() == '?single.M'
}

fn test_active_generic_parameter_shadows_same_named_concrete_type() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.cur_file = 'generic_shadow.v'
	tc.structs[flat.source_type_id('T')] = []StructField{}
	assert tc.parse_type(flat.source_type_id('T')) is Struct
	tc.fn_context.generic_params = ['T']
	assert tc.parse_type(flat.source_type_id('T')) is Unknown
	for spelling in ['[]T', '&T', '?T', '!T', 'map[string]T', 'fn (T) T'] {
		assert tc.parse_type(flat.source_type_id(spelling)).name().contains('unknown'), spelling
	}
	assert tc.parse_type(flat.source_type_id('main.T')) is Struct
	tc.fn_context.generic_params = []string{}
	assert tc.parse_type(flat.source_type_id('T')) is Struct
}
