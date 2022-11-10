interface Widget {}

struct Rectangle {}

struct Children {
mut:
	children map[string]Widget
}

fn rectangle() &Rectangle {
	return &Rectangle{}
}

fn test_map_of_interfaces_infers_interface_casts_of_the_map_values() {
	rect := rectangle()
	mut m := map[string]Widget{}
	m = {
		'abc': rect
		'def': rectangle()
	}
	res := m.str()
	assert res == "{'abc': Widget(Rectangle{}), 'def': Widget(Rectangle{})}"
}

fn test_map_of_interfaces_infers_casts_of_the_map_values_when_assigned_to_a_struct_field_too() {
	mut all := &Children{}
	rect := rectangle()
	all.children = {
		'abc': rect
		'def': rectangle()
	}
	res := all.children.str()
	assert res == "{'abc': Widget(Rectangle{}), 'def': Widget(Rectangle{})}"
}
