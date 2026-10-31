module naming

fn test_sum_fields_preserve_pointer_depth() {
	variants := ['int', '&int', '&&int', 'model.Item', '&model.Item']
	expected := [
		'_int',
		'_ptr__int',
		'_ptr__ptr__int',
		'model__Item',
		'_ptr_model__Item',
	]
	for i, variant in variants {
		assert sum_field_name(variant) == expected[i]
	}
}

fn test_sum_function_fields_use_signature_identity() {
	assert sum_field_name('fn (value int) string') == sum_field_name('fn(int) string')
	assert sum_field_name('fn (mut value int)') != sum_field_name('fn(int)')
	assert sum_field_name('fn(int) string') != sum_field_name('fn(int) int')
}
