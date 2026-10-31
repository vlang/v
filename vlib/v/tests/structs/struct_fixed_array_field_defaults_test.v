struct FixedDefaults {
mut:
	maps   [2]map[string]int
	arrays [2][]int
}

struct FixedDefaultsOuter {
mut:
	inner FixedDefaults
}

fn make_fixed_defaults[T]() T {
	return T{}
}

fn check_fixed_defaults(mut d FixedDefaults) {
	d.maps[0]['a'] = 1
	d.maps[1]['b'] = 2
	d.arrays[1] << 3
	assert d.maps[0]['a'] == 1
	assert d.maps[1]['b'] == 2
	assert d.arrays[0].len == 0
	assert d.arrays[1] == [3]
}

// A struct's zero value has to set up the maps and dynamic arrays inside its
// fixed-array fields, like it does for plain map and array fields.
fn test_struct_literal_initializes_fixed_array_elements() {
	mut d := FixedDefaults{}
	check_fixed_defaults(mut d)
}

fn test_generic_zero_value_initializes_fixed_array_elements() {
	mut d := make_fixed_defaults[FixedDefaults]()
	check_fixed_defaults(mut d)
}

fn test_nested_struct_initializes_fixed_array_elements() {
	mut outer := FixedDefaultsOuter{}
	check_fixed_defaults(mut outer.inner)
}

fn test_array_elements_initialize_fixed_array_fields() {
	mut list := []FixedDefaults{len: 2}
	check_fixed_defaults(mut list[1])
}
