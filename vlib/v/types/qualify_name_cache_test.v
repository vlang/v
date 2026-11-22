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
	tc.structs[flat.source_type_id('generic.Cell')] = []StructField{}
	assert tc.qualify_name('Cell') == 'generic.Cell'
	assert tc.qualify_name('[]Cell') == '[]generic.Cell'
	tc.cur_module = 'caller'
	tc.cur_file = 'caller.v'
	assert tc.qualify_name('Cell') == 'Cell'
	tc.structs[flat.source_type_id('caller.Cell')] = []StructField{}
	assert tc.qualify_name('[]Cell') == '[]caller.Cell'
	tc.cur_module = 'generic'
	tc.cur_file = 'generic.v'
	assert tc.qualify_name('[]Cell') == '[]generic.Cell'
	for name in ['int', 'string', 'voidptr', 'u128'] {
		assert tc.qualify_name(name) == tc.qualify_name_uncached(name)
	}
}

@[unsafe]
fn test_numeric_qualification_preserves_builtin_set_and_string_cache_interoperation() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.qualify_name_cache = &QualifyNameCache{}
	tc.cur_module = 'names'
	tc.cur_file = 'names.v'
	for name in ['i8', 'u8', 'int', 'i16', 'i32', 'i64', 'u16', 'u32', 'u64', 'f32', 'f64', 'map',
		'nil', 'bool', 'char', 'i128', 'rune', 'u128', 'uint', 'void', 'none', 'isize', 'usize',
		'array', 'string', 'voidptr', 'charptr', 'byteptr'] {
		id := flat.source_type_id(name)
		assert tc.qualify_type_name(id) == id
		assert tc.qualify_name(name) == name
	}
	for name in ['unknown', 'generic', 'Cell', ' int', ''] {
		id := flat.source_type_id(name)
		qualified := tc.qualify_type_name(id)
		assert flat.source_type_name(qualified) == 'names.${name}'
		assert tc.qualify_name(name) == flat.source_type_name(qualified)
		assert tc.qualify_type_name(id) == qualified
	}
	tc.qualify_name_cache = unsafe { nil }
	assert tc.qualify_type_name(flat.source_type_id('Cell')) == flat.source_type_id('names.Cell')
	assert tc.qualify_type_name(flat.source_type_u64) == flat.source_type_u64
}

fn test_numeric_qualification_keeps_file_imports_separate() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.qualify_name_cache = &QualifyNameCache{}
	tc.cur_module = 'main'
	tc.structs[flat.source_type_id('one.Cell')] = []StructField{}
	tc.structs[flat.source_type_id('two.Cell')] = []StructField{}
	tc.cur_file = 'one.v'
	tc.register_file_import('dep', 'one')
	tc.cur_file = 'two.v'
	tc.register_file_import('dep', 'two')
	id := flat.source_type_id('dep.Cell')
	array_id := flat.source_type_id('[]dep.Cell')
	for _ in 0 .. 3 {
		tc.cur_file = 'one.v'
		assert tc.qualify_type_name(id) == flat.source_type_id('one.Cell')
		assert tc.qualify_name('dep.Cell') == 'one.Cell'
		assert tc.qualify_type_name(array_id) == flat.source_type_id('[]one.Cell')
		tc.cur_file = 'two.v'
		assert tc.qualify_name('dep.Cell') == 'two.Cell'
		assert tc.qualify_type_name(id) == flat.source_type_id('two.Cell')
		assert tc.qualify_type_name(array_id) == flat.source_type_id('[]two.Cell')
	}
}

fn test_numeric_qualification_invalidates_for_module_mode_and_late_types() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.qualify_name_cache = &QualifyNameCache{}
	tc.resolution_type_mode = true
	tc.cur_file = 'shared.v'
	tc.cur_module = 'generic'
	id := flat.source_type_id('Cell')
	array_id := flat.source_type_id('[]Cell')
	assert tc.qualify_type_name(id) == id
	assert tc.qualify_type_name(array_id) == array_id
	tc.structs[flat.source_type_id('generic.Cell')] = []StructField{}
	assert tc.qualify_type_name(id) == flat.source_type_id('generic.Cell')
	assert tc.qualify_type_name(array_id) == flat.source_type_id('[]generic.Cell')
	tc.cur_module = 'caller'
	assert tc.qualify_type_name(id) == id
	tc.resolution_type_mode = false
	assert tc.qualify_type_name(id) == flat.source_type_id('caller.Cell')
	tc.resolution_type_mode = true
	assert tc.qualify_type_name(id) == id
	tc.structs[flat.source_type_id('caller.Cell')] = []StructField{}
	assert tc.qualify_type_name(array_id) == flat.source_type_id('[]caller.Cell')
	tc.cur_module = 'generic'
	assert tc.qualify_type_name(array_id) == flat.source_type_id('[]generic.Cell')
}

fn test_numeric_resolution_qualification_preserves_global_fallback_and_local_priority() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.qualify_name_cache = &QualifyNameCache{}
	tc.cur_module = 'generic'
	tc.cur_file = 'generic.v'
	global_id := flat.source_type_id('Cell')
	local_id := flat.source_type_id('generic.Cell')
	tc.structs[flat.source_type_id('Cell')] = []StructField{}
	// The resolution helper keeps an existing capitalized global even while
	// ordinary registration qualification still adds the current module.
	assert tc.qualify_type_name(global_id) == local_id
	assert tc.qualify_resolution_type_name(global_id) == global_id
	assert tc.qualify_resolution_type_name(global_id) == global_id
	tc.structs[flat.source_type_id('generic.Cell')] = []StructField{}
	assert tc.qualify_resolution_type_name(global_id) == local_id
	tc.structs[flat.source_type_id('lower')] = []StructField{}
	assert tc.qualify_resolution_type_name(flat.source_type_id('lower')) == flat.source_type_id('generic.lower')
	assert tc.qualify_resolution_type_name(flat.source_type_id('Missing')) == flat.source_type_id('generic.Missing')
	assert tc.qualify_resolution_type_name(u32(0)) == flat.source_type_id('generic.')
	assert tc.qualify_resolution_type_name(flat.source_type_int) == flat.source_type_int
	tc.resolution_type_mode = true
	missing_id := flat.source_type_id('Missing')
	assert tc.qualify_resolution_type_name(missing_id) == missing_id
	tc.cur_module = 'main'
	assert tc.qualify_resolution_type_name(global_id) == global_id
}
