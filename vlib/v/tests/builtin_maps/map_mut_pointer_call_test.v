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

struct MapMutPointerValue[T] {
mut:
	value T
}

fn (mut item MapMutPointerValue[T]) set(value T) !bool {
	assert item.value == 3
	item.value = value
	return true
}

fn map_pointer_value_token(name string) !u32 {
	assert name == 'one'
	return 11
}

fn replace_map_pointer_value(mut items map[string]&MapMutPointerValue[u32], name string, replacement &MapMutPointerValue[u32]) !u32 {
	items[name] = replacement
	return 11
}

fn replace_array_pointer_value(mut items []&MapMutPointerValue[u32], replacement &MapMutPointerValue[u32]) !u32 {
	items[0] = replacement
	return 11
}

fn test_mut_map_generic_pointer_receiver_precedes_fallible_argument() ! {
	original := &MapMutPointerValue[u32]{ value: 3 }
	mut items := {
		'one': original
	}
	for name, mut item in items {
		item.set(map_pointer_value_token(name)!)!
	}
	assert original.value == 11
}

fn test_mut_array_generic_pointer_receiver_precedes_fallible_argument() ! {
	original := &MapMutPointerValue[u32]{ value: 3 }
	mut items := [original]
	for mut item in items {
		item.set(map_pointer_value_token('one')!)!
	}
	assert original.value == 11
}

fn test_mut_map_generic_pointer_receiver_captures_pointee_before_entry_replacement() ! {
	original := &MapMutPointerValue[u32]{ value: 3 }
	replacement := &MapMutPointerValue[u32]{ value: 99 }
	mut items := {
		'one': original
	}
	for name, mut item in items {
		item.set(if name == 'one' {
			replace_map_pointer_value(mut items, name, replacement)!
		} else {
			u32(0)
		})!
	}
	assert original.value == 11
	assert replacement.value == 99
	stored := items['one'] or { panic('missing item') }
	assert stored == replacement
}

fn test_mut_array_generic_pointer_receiver_captures_pointee_before_entry_replacement() ! {
	original := &MapMutPointerValue[u32]{ value: 3 }
	replacement := &MapMutPointerValue[u32]{ value: 99 }
	mut items := [original]
	for mut item in items {
		item.set(if items.len == 1 {
			replace_array_pointer_value(mut items, replacement)!
		} else {
			u32(0)
		})!
	}
	assert original.value == 11
	assert replacement.value == 99
	assert items[0] == replacement
}

fn test_mut_map_generic_struct_receiver_keeps_entry_storage_with_fallible_argument() ! {
	mut items := {
		'one': MapMutPointerValue[u32]{ value: 3 }
	}
	for name, mut item in items {
		item.set(map_pointer_value_token(name)!)!
	}
	assert items['one'].value == 11
}

fn test_mut_array_generic_struct_receiver_keeps_entry_storage_with_fallible_argument() ! {
	mut items := [MapMutPointerValue[u32]{ value: 3 }]
	for mut item in items {
		item.set(map_pointer_value_token('one')!)!
	}
	assert items[0].value == 11
}

fn rebind_map_pointer_item[T](mut target T, replacement T, marker int) {
	assert marker == 1
	target = replacement
}

fn test_mut_map_pointer_argument_keeps_entry_slot_before_if_argument() {
	original := &MapMutPointerItem{ value: 3 }
	replacement := &MapMutPointerItem{ value: 99 }
	mut items := {
		'one': original
	}
	for name, mut item in items {
		rebind_map_pointer_item[&MapMutPointerItem](mut item, replacement, if name == 'one' {
			1
		} else {
			2
		})
	}
	stored := items['one'] or { panic('missing item') }
	assert stored == replacement
	assert original.value == 3
}

fn test_mut_map_pointer_argument_keeps_entry_slot_before_match_argument() {
	original := &MapMutPointerItem{ value: 3 }
	replacement := &MapMutPointerItem{ value: 99 }
	mut items := {
		'one': original
	}
	for name, mut item in items {
		rebind_map_pointer_item[&MapMutPointerItem](mut item, replacement, match name {
			'one' { 1 }
			else { 2 }
		})
	}
	stored := items['one'] or { panic('missing item') }
	assert stored == replacement
	assert original.value == 3
}

fn test_mut_array_pointer_argument_keeps_entry_slot_before_if_argument() {
	original := &MapMutPointerItem{ value: 3 }
	replacement := &MapMutPointerItem{ value: 99 }
	mut items := [original]
	for mut item in items {
		rebind_map_pointer_item[&MapMutPointerItem](mut item, replacement, if items.len == 1 {
			1
		} else {
			2
		})
	}
	assert items[0] == replacement
	assert original.value == 3
}

fn test_mut_array_pointer_argument_keeps_entry_slot_before_match_argument() {
	original := &MapMutPointerItem{ value: 3 }
	replacement := &MapMutPointerItem{ value: 99 }
	mut items := [original]
	for mut item in items {
		rebind_map_pointer_item[&MapMutPointerItem](mut item, replacement, match items.len {
			1 { 1 }
			else { 2 }
		})
	}
	assert items[0] == replacement
	assert original.value == 3
}

fn set_map_pointer_item(mut item MapMutPointerValue[u32], value u32) {
	assert item.value == 3
	item.value = value
}

fn test_mut_map_pointer_argument_captures_pointee_before_entry_replacement() ! {
	original := &MapMutPointerValue[u32]{ value: 3 }
	replacement := &MapMutPointerValue[u32]{ value: 99 }
	mut items := {
		'one': original
	}
	for name, mut item in items {
		set_map_pointer_item(mut item, if name == 'one' {
			replace_map_pointer_value(mut items, name, replacement)!
		} else {
			u32(0)
		})
	}
	assert original.value == 11
	assert replacement.value == 99
	stored := items['one'] or { panic('missing item') }
	assert stored == replacement
}

fn test_mut_array_pointer_argument_captures_pointee_before_entry_replacement() ! {
	original := &MapMutPointerValue[u32]{ value: 3 }
	replacement := &MapMutPointerValue[u32]{ value: 99 }
	mut items := [original]
	for mut item in items {
		set_map_pointer_item(mut item, if items.len == 1 {
			replace_array_pointer_value(mut items, replacement)!
		} else {
			u32(0)
		})
	}
	assert original.value == 11
	assert replacement.value == 99
	assert items[0] == replacement
}

// vfmt off
fn test_mut_map_pointer_argument_keeps_entry_slot_before_if_argument_in_parens() {
	original := &MapMutPointerItem{ value: 3 }
	replacement := &MapMutPointerItem{ value: 99 }
	mut items := {
		'one': original
	}
	for name, mut item in items {
		rebind_map_pointer_item[&MapMutPointerItem](mut ((item)), replacement, if name == 'one' {
			1
		} else {
			2
		})
	}
	stored := items['one'] or { panic('missing item') }
	assert stored == replacement
	assert original.value == 3
}

fn test_mut_array_pointer_argument_keeps_entry_slot_before_match_argument_in_parens() {
	original := &MapMutPointerItem{ value: 3 }
	replacement := &MapMutPointerItem{ value: 99 }
	mut items := [original]
	for mut item in items {
		rebind_map_pointer_item[&MapMutPointerItem](mut ((item)), replacement, match items.len {
			1 { 1 }
			else { 2 }
		})
	}
	assert items[0] == replacement
	assert original.value == 3
}

fn test_mut_map_pointer_argument_captures_pointee_before_entry_replacement_in_parens() ! {
	original := &MapMutPointerValue[u32]{ value: 3 }
	replacement := &MapMutPointerValue[u32]{ value: 99 }
	mut items := {
		'one': original
	}
	for name, mut item in items {
		set_map_pointer_item(mut ((item)), if name == 'one' {
			replace_map_pointer_value(mut items, name, replacement)!
		} else {
			u32(0)
		})
	}
	assert original.value == 11
	assert replacement.value == 99
	stored := items['one'] or { panic('missing item') }
	assert stored == replacement
}

fn test_mut_array_pointer_argument_captures_pointee_before_entry_replacement_in_parens() ! {
	original := &MapMutPointerValue[u32]{ value: 3 }
	replacement := &MapMutPointerValue[u32]{ value: 99 }
	mut items := [original]
	for mut item in items {
		set_map_pointer_item(mut ((item)), if items.len == 1 {
			replace_array_pointer_value(mut items, replacement)!
		} else {
			u32(0)
		})
	}
	assert original.value == 11
	assert replacement.value == 99
	assert items[0] == replacement
}
// vfmt on

fn rebind_map_pointer_callback(mut target &MapMutPointerItem, replacement &MapMutPointerItem, marker int) {
	target = replacement
	assert marker == 1
}

// vfmt off
fn test_mut_array_pointer_callback_keeps_entry_slot_before_match_argument() {
	original := &MapMutPointerItem{ value: 3 }
	replacement := &MapMutPointerItem{ value: 99 }
	mut items := [original]
	callback := rebind_map_pointer_callback
	for mut item in items {
		callback(mut ((item)), replacement, match items.len {
			1 { 1 }
			else { 2 }
		})
	}
	assert items[0] == replacement
	assert original.value == 3
}

fn test_mut_map_pointer_callback_keeps_entry_slot_before_if_argument() {
	original := &MapMutPointerItem{ value: 3 }
	replacement := &MapMutPointerItem{ value: 99 }
	mut items := { 'one': original }
	callback := rebind_map_pointer_callback
	for name, mut item in items {
		callback(mut ((item)), replacement, if name == 'one' { 1 } else { 2 })
	}
	stored := items['one'] or { panic('missing item') }
	assert stored == replacement
	assert original.value == 3
}
// vfmt on
