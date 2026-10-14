module types

import v.flat

fn test_qualification_cache_preserves_context_and_late_generic_declarations() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.qualify_name_cache = &QualifyNameCache{}
	tc.resolution_type_mode = true
	tc.cur_module = 'generic'
	tc.cur_file = 'generic.v'
	assert tc.qualify_name('Cell') == 'Cell'
	assert tc.qualify_name('[]Cell') == '[]Cell'
	tc.structs['generic.Cell'] = []StructField{}
	assert tc.qualify_name('Cell') == 'generic.Cell'
	assert tc.qualify_name('[]Cell') == '[]generic.Cell'
	tc.cur_module = 'caller'
	tc.cur_file = 'caller.v'
	assert tc.qualify_name('Cell') == 'Cell'
	tc.structs['caller.Cell'] = []StructField{}
	assert tc.qualify_name('[]Cell') == '[]caller.Cell'
	tc.cur_module = 'generic'
	tc.cur_file = 'generic.v'
	assert tc.qualify_name('[]Cell') == '[]generic.Cell'
	for name in ['int', 'string', 'voidptr', 'u128'] {
		assert tc.qualify_name(name) == tc.qualify_name_uncached(name)
	}
}
