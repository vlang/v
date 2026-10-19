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

	assert tc.parse_type(flat.source_type_id('foo.Box[item.Value]')).name() == 'x.foo.Box[x.item.Value]'
	assert tc.parse_canonical_type(flat.source_type_id('foo.Box[item.Value]')).name() == 'foo.Box[item.Value]'
}

fn test_parse_canonical_builtin_type_ignores_registered_struct() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['string'] = []StructField{}

	assert tc.parse_canonical_type(flat.source_type_string) is String
}

fn test_parse_canonical_type_cached_does_not_reuse_typeof_across_scopes() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	tc.cur_scope = new_scope(tc.file_scope)
	tc.cur_scope.insert('x', Type(int_))
	assert tc.parse_canonical_type_cached(flat.source_type_id('[]typeof(x)')).name() == '[]int'
	// Same text and parse context, but `x` now has another type in scope.
	tc.cur_scope = new_scope(tc.file_scope)
	tc.cur_scope.insert('x', Type(string_))
	assert tc.parse_canonical_type_cached(flat.source_type_id('[]typeof(x)')).name() == '[]string'
	// Context-independent texts are still memoized.
	assert tc.parse_canonical_type_cached(flat.source_type_id('[]int')).name() == '[]int'
	assert flat.source_type_id('[]int') in tc.type_cache.canonical_texts
	assert flat.source_type_id('[]typeof(x)') !in tc.type_cache.canonical_texts
}
