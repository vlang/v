interface Named {
	name string
}

struct NamedValue {
mut:
	name string
}

fn test_interface_cast_makes_detached_copy() {
	mut value := NamedValue{
		name: 'First'
	}
	face := Named(value)
	value.name = 'Second'
	assert face.name == 'First'
}

fn test_append_to_interface_array_makes_detached_copy() {
	mut value := NamedValue{
		name: 'First'
	}
	mut values := []Named{}
	values << value
	value.name = 'Second'
	assert values[0].name == 'First'
}

fn new_named_value() !NamedValue {
	return NamedValue{
		name: 'First'
	}
}

fn read_name(n Named) string {
	return n.name
}

fn test_auto_heap_interface_arg_keeps_value_fields() {
	value := new_named_value()!
	face := Named(value)
	assert read_name(value) == 'First'
	assert read_name(face) == 'First'
}

interface IndexedReader {
	value int
	read() int
}

struct IndexedPayload {
mut:
	value int
	data  [128]u64
}

fn (p &IndexedPayload) read() int {
	return p.value + int(p.data[127])
}

fn next_box_index(mut calls []int) int {
	calls << calls.len
	return 0
}

fn box_dynamic_element(values []IndexedPayload, mut calls []int) IndexedReader {
	return IndexedReader(values[next_box_index(mut calls)])
}

fn box_fixed_element(values &[1]IndexedPayload, mut calls []int) IndexedReader {
	return values[next_box_index(mut calls)]
}

fn test_indexed_interface_box_owns_its_value() {
	mut calls := []int{}
	mut dynamic := [IndexedPayload{ value: 10 }]
	dynamic[0].data[127] = 1
	mut fixed := [IndexedPayload{ value: 20 }]!
	fixed[0].data[127] = 2

	dynamic_box := box_dynamic_element(dynamic, mut calls)
	fixed_box := box_fixed_element(&fixed, mut calls)
	dynamic[0] = IndexedPayload{}
	fixed[0] = IndexedPayload{}

	assert calls == [0, 1]
	assert dynamic_box.value == 10
	assert dynamic_box.read() == 11
	assert fixed_box.value == 20
	assert fixed_box.read() == 22
}
