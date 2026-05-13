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
