module transform

import v.flat

fn test_numeric_declaration_type_eligibility_preserves_provisional_spellings() {
	assert !decl_type_is_usable(0)
	for text in ['unknown', 'array', 'map', 'struct', '?unknown', 'Option_int', '__v_option',
		'R esult _int', 'typeof(value)'] {
		assert !decl_type_is_usable(flat.source_type_id(text)), text
	}
	for text in ['void', '?int', 'pkg.Item', 'array ', 'unk nown', '\tOption', 'Option[T]', 'typeof(',
		'typeof (value)'] {
		assert decl_type_is_usable(flat.source_type_id(text)), text
	}
}
