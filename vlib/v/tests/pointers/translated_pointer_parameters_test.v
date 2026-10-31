@[translated]
module main

type TranslatedIntPtr = &int

fn advance_translated_pointer(mut cursor TranslatedIntPtr) int {
	value := int(**cursor)
	unsafe {
		*cursor = &int(*cursor) + 1
	}
	return value
}

fn test_translated_pointer_alias_parameter() {
	values := [11, 22, 33]!
	mut cursor := unsafe { &values[0] }
	assert advance_translated_pointer(mut cursor) == 11
	assert *cursor == 22
	assert advance_translated_pointer(mut cursor) == 22
	assert *cursor == 33
	assert values == [11, 22, 33]!
}
