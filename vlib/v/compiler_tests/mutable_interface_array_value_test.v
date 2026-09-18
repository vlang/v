interface MutableInterfaceArrayValue {
	value() int
}

struct MutableInterfaceArrayItem {
	n int
}

fn (item MutableInterfaceArrayItem) value() int {
	return item.n
}

fn read_mutable_interface(mut item MutableInterfaceArrayValue) int {
	return item.value()
}

fn test_mutable_interface_array_value_passes_storage_pointer() {
	mut values := []MutableInterfaceArrayValue{}
	values << MutableInterfaceArrayItem{
		n: 7
	}
	mut result := 0
	for mut value in values {
		result = read_mutable_interface(mut value)
	}
	assert result == 7
}
