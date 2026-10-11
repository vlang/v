type AccessorValue = []AccessorValue | int

fn first_accessor_value(value AccessorValue) AccessorValue {
	return if value is []AccessorValue { value.first() } else { value }
}

fn last_accessor_value(value AccessorValue) AccessorValue {
	return if value is []AccessorValue { value.last() } else { value }
}

fn test_array_accessors_preserve_sum_element_types() {
	first := AccessorValue(1)
	last := AccessorValue(2)
	values := AccessorValue([first, last])
	assert first_accessor_value(values) == first
	assert last_accessor_value(values) == last
	assert first_accessor_value(last) == last
	assert last_accessor_value(first) == first
}

fn append_accessor_value(mut value AccessorValue, item AccessorValue) {
	if mut value !is []AccessorValue {
		return
	}
	value << item
}

fn test_mutable_array_projection_after_exiting_guard() {
	first := AccessorValue(1)
	last := AccessorValue(2)
	mut value := AccessorValue([first])
	append_accessor_value(mut value, last)
	assert last_accessor_value(value) == last
	if mut value !is []AccessorValue {
		assert false
		return
	}
	value << first
	assert value.len == 3
	value = AccessorValue([last])
	assert value == AccessorValue([last])
	value = AccessorValue(7)
	assert value == AccessorValue(7)
}

fn test_scalar_payload_mutation_preserves_tag() {
	mut value := AccessorValue(1)
	if mut value !is int {
		assert false
		return
	}
	value++
	assert value == 2
	value = AccessorValue([]AccessorValue{})
	assert value is []AccessorValue
}
