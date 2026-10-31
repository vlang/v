module types

import v.flat

fn test_constraint_type_selection_preserves_spelling_identity_and_order() {
	name_id := flat.source_type_id('ConstraintSpelling')
	first := Type(Alias{ name_id: name_id, base_type: &builtin_int_type })
	// Selection has always matched spellings, including aliases with different bases.
	same_spelling := Type(Alias{ name_id: name_id, base_type: &builtin_string_type })
	other := Type(Alias{ name_id: flat.source_type_id('OtherConstraintSpelling'), base_type: &builtin_int_type })
	values := [first, other, first, builtin_bool_type]
	assert types_within(values, [same_spelling]) == [first, first]
	assert types_without(values, [same_spelling]) == [other, builtin_bool_type]
	assert types_within(values, [other, builtin_bool_type]) == [other, builtin_bool_type]
	assert types_without(values, [other, builtin_bool_type]) == [first, first]
}
