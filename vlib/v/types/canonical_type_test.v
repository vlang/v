module types

import v.flat

fn test_parse_canonical_generic_type_ignores_source_import_aliases() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'main.v'
	tc.cur_module = 'main'
	tc.structs['foo.Box'] = []StructField{}
	tc.structs['x.foo.Box'] = []StructField{}
	tc.structs['item.Value'] = []StructField{}
	tc.structs['x.item.Value'] = []StructField{}
	tc.struct_generic_params['foo.Box'] = ['T']
	tc.struct_generic_params['x.foo.Box'] = ['T']
	tc.register_file_import('foo', 'x.foo')
	tc.register_file_import('item', 'x.item')

	assert tc.parse_type('foo.Box[item.Value]').name() == 'x.foo.Box[x.item.Value]'
	assert tc.parse_canonical_type('foo.Box[item.Value]').name() == 'foo.Box[item.Value]'
}

fn test_parse_canonical_builtin_type_ignores_registered_struct() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['string'] = []StructField{}

	assert tc.parse_canonical_type('string') is String
}
