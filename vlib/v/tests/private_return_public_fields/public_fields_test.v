import opaque

fn test_public_fields_of_inferred_private_return_type() ! {
	record := opaque.new_record()!
	assert record.value == 42
	assert record.get_value() == 42
	for _, value in opaque.records() {
		assert value.value == 7
	}
}
