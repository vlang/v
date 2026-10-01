type PointerValue = int | []PointerValue | map[string]PointerValue

fn test_sumtype_cast_from_map_value_reference() {
	unsafe {
		mut table := &map[string]PointerValue{}
		table['answer'] = 42
		value := PointerValue(table)
		result := value as map[string]PointerValue
		assert (result['answer'] or { panic('missing answer') }) == PointerValue(42)
	}
}

fn test_sumtype_cast_from_array_value_reference() {
	array := &[PointerValue(42)]
	value := PointerValue(array)
	assert (value as []PointerValue)[0] == PointerValue(42)
}

fn test_sumtype_cast_from_int_value_reference() {
	answer := 42
	value := PointerValue(&answer)
	assert (value as int) == 42
}
