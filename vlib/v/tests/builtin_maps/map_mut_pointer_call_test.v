struct MapMutPointerItem {
mut:
	value int
}

fn increment_map_pointer_item(mut item MapMutPointerItem) {
	assert item.value == 3
	item.value++
}

fn (mut item MapMutPointerItem) increment() {
	item.value++
}

fn test_mut_map_pointer_value_passes_the_pointee_to_mut_struct_parameter() {
	original := &MapMutPointerItem{ value: 3 }
	mut items := {
		'one': original
	}
	for _, mut item in items {
		increment_map_pointer_item(mut item)
		item.increment()
	}
	assert original.value == 5
	stored := items['one'] or { panic('missing item') }
	assert stored == original
}

fn test_mut_map_struct_value_keeps_mutating_the_entry_storage() {
	mut items := {
		'one': MapMutPointerItem{ value: 3 }
	}
	for _, mut item in items {
		increment_map_pointer_item(mut item)
		item.increment()
	}
	assert items['one'].value == 5
}

fn test_mut_map_pointer_value_passes_the_pointee_to_callable_variable() {
	update := increment_map_pointer_item
	original := &MapMutPointerItem{ value: 3 }
	mut items := {
		'one': original
	}
	for _, mut item in items {
		update(mut item)
	}
	assert original.value == 4
	stored := items['one'] or { panic('missing item') }
	assert stored == original
}
