module types

import v.flat

fn test_generic_base_type_id_preserves_arrays_and_unapplied_spelling() {
	for name in ['', 'pkg.Item', '[]Box[int]', '[2]Box[int]'] {
		name_id := flat.source_type_id(name)
		assert generic_base_type_id(name_id) == name_id, name
	}
	assert generic_base_type_id(flat.source_type_id('pkg.Box[[]map[string]Pair[int, string]]')) == flat.source_type_id('pkg.Box')
	assert generic_base_type_id(flat.source_type_id('&pkg.Box[int]')) == flat.source_type_id('&pkg.Box')
}

fn test_builtin_type_index_keeps_runtime_numbering_without_normalizing_spelling() {
	assert builtin_type_index(flat.source_type_void) == 1
	assert builtin_type_index(flat.source_type_int) == 8
	assert builtin_type_index(flat.source_type_isize) == 10
	assert builtin_type_index(flat.source_type_rune) == 22
	assert builtin_type_index(flat.source_type_id('float literal')) == 27
	assert builtin_type_index(flat.source_type_id('int literal')) == 28
	assert builtin_type_index(flat.source_type_id('thread')) == 29
	assert builtin_type_index(flat.source_type_nil) == 31
	assert builtin_type_index(flat.source_type_id(' int')) == 0
	assert builtin_type_index(flat.source_type_id('pkg.int')) == 0
	assert builtin_type_index(0) == 0
}

fn test_type_index_trims_each_pointer_layer_without_changing_runtime_identity() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	assert tc.type_index(flat.source_type_id(' \t& & int \n')) == 8 | (2 << 16)
	assert tc.type_index(flat.source_type_id(' \tint \n')) == 8
	assert tc.type_index(flat.source_type_id(' \t\n')) == 0
	assert tc.type_index(flat.source_type_id(' & & pkg.Item ')) == tc.type_index(flat.source_type_id('pkg.Item')) | (2 << 16)
}
