struct ReferenceSettings {
	value int
	label string
}

fn read_reference_settings(value &ReferenceSettings) string {
	return '${value.label}:${value.value}'
}

fn read_nested_reference_settings(value &&ReferenceSettings) string {
	return read_reference_settings(*value)
}

fn test_struct_update_as_reference_argument() {
	original := ReferenceSettings{ value: 3, label: 'original' }
	assert read_reference_settings(ReferenceSettings{ ...original, value: 7 }) == 'original:7'
	assert read_nested_reference_settings(ReferenceSettings{ ...original, label: 'copy' }) == 'copy:3'
	assert read_reference_settings(original) == 'original:3'
}

fn updated_settings_array(value int) []&ReferenceSettings {
	original := ReferenceSettings{ value: -1, label: 'array' }
	return [ReferenceSettings{ ...original, value: value }]
}

fn updated_settings_nested_array(value int) []&&ReferenceSettings {
	original := ReferenceSettings{ value: -1, label: 'nested' }
	return [ReferenceSettings{ ...original, value: value }]
}

fn test_struct_updates_stored_as_pointers_keep_distinct_storage() {
	mut values := []&ReferenceSettings{}
	for value in 0 .. 8 {
		values << updated_settings_array(value)
	}
	assert values.len == 8
	for i, value in values {
		assert value.value == i
		assert value.label == 'array'
		if i > 0 {
			assert value != values[i - 1]
		}
	}
}

fn test_struct_updates_stored_as_nested_pointers_keep_distinct_storage() {
	mut values := []&&ReferenceSettings{}
	for value in 0 .. 8 {
		values << updated_settings_nested_array(value)
	}
	assert values.len == 8
	for i, value in values {
		assert (**value).value == i
		assert (**value).label == 'nested'
		if i > 0 {
			assert value != values[i - 1]
		}
	}
}
