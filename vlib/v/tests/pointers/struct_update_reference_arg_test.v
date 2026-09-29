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

fn updated_settings_parenthesized_array(base ReferenceSettings, value int) []&ReferenceSettings {
	return [(ReferenceSettings{ ...base, value: value })]
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

fn test_parenthesized_struct_updates_stored_as_pointers() {
	mut values := []&ReferenceSettings{}
	base := ReferenceSettings{ value: -1, label: 'parenthesized' }
	for value in 0 .. 8 {
		values << updated_settings_parenthesized_array(base, value)
	}
	for i, value in values {
		assert value.value == i
		assert value.label == 'parenthesized'
	}
}

struct ReferenceNodeA {
	value int
}

struct ReferenceNodeB {
	value int
}

type ReferenceNode = ReferenceNodeA | ReferenceNodeB
type ReferenceInnerNode = ReferenceNodeA | ReferenceNodeB

struct ReferenceNodeC {
	value int
}

type ReferenceOuterNode = ReferenceInnerNode | ReferenceNodeC

fn updated_sum_array(base ReferenceNodeA, value int) []&ReferenceNode {
	return [ReferenceNodeA{ ...base, value: value }]
}

fn updated_nested_sum_array(base ReferenceNodeA, value int) []&&ReferenceNode {
	return [ReferenceNodeA{ ...base, value: value }]
}

fn updated_nested_outer_sum_array(base ReferenceNodeA, value int) []&&ReferenceOuterNode {
	return [ReferenceNodeA{ ...base, value: value }]
}

fn test_struct_update_stored_as_sum_pointer() {
	base := ReferenceNodeA{ value: -1 }
	mut values := []&ReferenceNode{}
	for value in 0 .. 8 {
		values << updated_sum_array(base, value)
	}
	for i, value in values {
		node := *value
		match node {
			ReferenceNodeA {
				assert node.value == i
			}
			else {
				assert false
			}
		}
	}
}

fn test_struct_update_stored_as_nested_sum_pointer() {
	base := ReferenceNodeA{ value: -1 }
	mut values := []&&ReferenceNode{}
	for value in 0 .. 8 {
		values << updated_nested_sum_array(base, value)
	}
	for i, value in values {
		node := **value
		match node {
			ReferenceNodeA {
				assert node.value == i
			}
			else {
				assert false
			}
		}
	}
}

fn test_struct_update_stored_as_nested_sum_variant_pointer() {
	base := ReferenceNodeA{ value: -1 }
	mut values := []&&ReferenceOuterNode{}
	for value in 0 .. 8 {
		values << updated_nested_outer_sum_array(base, value)
	}
	for i, value in values {
		node := **value
		match node {
			ReferenceInnerNode {
				match node {
					ReferenceNodeA {
						assert node.value == i
					}
					else {
						assert false
					}
				}
			}
			else {
				assert false
			}
		}
	}
}

interface ReferenceReader {
	read() int
}

type ReferenceReaderRefs = &&ReferenceReader

struct ReferenceReaderImpl {
	value int
}

fn (value ReferenceReaderImpl) read() int {
	return value.value
}

fn updated_interface_array(base ReferenceReaderImpl) []&&ReferenceReader {
	return [ReferenceReaderImpl{ ...base, value: base.value + 1 }]
}

fn updated_triple_interface_array(base ReferenceReaderImpl) []&&&ReferenceReader {
	return [ReferenceReaderImpl{ ...base, value: base.value + 2 }]
}

fn updated_aliased_interface_array(base ReferenceReaderImpl) []ReferenceReaderRefs {
	return [ReferenceReaderImpl{ ...base, value: base.value + 3 }]
}

fn test_struct_update_stored_as_nested_interface_pointer() {
	values := updated_interface_array(ReferenceReaderImpl{ value: 41 })
	assert values.len == 1
	assert values[0] == values[0]
	other := updated_interface_array(ReferenceReaderImpl{ value: 41 })
	assert values[0] != other[0]
	reader := **values[0]
	assert reader.read() == 42
	triple := updated_triple_interface_array(ReferenceReaderImpl{ value: 40 })
	triple_reader := ***triple[0]
	assert triple_reader.read() == 42
	aliased := updated_aliased_interface_array(ReferenceReaderImpl{ value: 39 })
	aliased_reader := **aliased[0]
	assert aliased_reader.read() == 42
}
