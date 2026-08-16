struct MapPointerValue {
mut:
	n int
}

fn test_mut_map_pointer_value_selector_uses_pointee_storage() {
	mut value := MapPointerValue{
		n: 1
	}
	mut values := {
		'value': &value
	}
	for _, mut item in values {
		item.n += 2
	}
	assert value.n == 3
}
