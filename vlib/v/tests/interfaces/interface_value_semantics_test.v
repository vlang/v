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
