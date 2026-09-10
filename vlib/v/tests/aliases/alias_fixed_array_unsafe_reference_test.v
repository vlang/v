type Arr = [8]int

fn test_alias_fixed_array_unsafe_reference() {
	a := Arr{}
	n := unsafe { &a }
	assert n != unsafe { nil }
}
