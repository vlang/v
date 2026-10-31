fn remove_nested_map_key(mut entries map[string]map[string]int, target string) {
	for key, values in entries {
		for inner_key in values.keys() {
			if inner_key == target {
				entries[key].delete(inner_key)
			}
		}
	}
}

fn test_map_methods_on_borrowed_iteration_values() {
	mut entries := {
		'first':  {
			'keep':   2
			'remove': 3
		}
		'second': {
			'keep': 5
		}
	}
	mut total := 0
	for _, values in entries {
		assert values.keys().len == values.len
		for value in values.values() {
			total += value
		}
	}
	assert total == 10
	remove_nested_map_key(mut entries, 'remove')
	assert entries['first'] == {
		'keep': 2
	}
	assert entries['second'] == {
		'keep': 5
	}
}

fn test_address_of_borrowed_map_iteration_value() {
	mut entries := {
		'a': {
			'value': 7
		}
	}
	for _, mut values in entries {
		pointer := &values
		assert pointer.keys() == ['value']
		assert pointer.values() == [7]
	}
}

fn test_methods_on_explicit_pointer_iteration_values() {
	mut inner := {
		'value': 7
	}
	entries := {
		'a': &inner
	}
	for _, value in entries {
		assert value.keys() == ['value']
		assert value.values() == [7]
	}
}
