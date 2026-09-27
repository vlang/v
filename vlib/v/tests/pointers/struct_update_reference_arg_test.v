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
