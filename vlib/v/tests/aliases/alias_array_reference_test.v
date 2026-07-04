type Arr = []int

fn test_alias_array_reference() {
	mut a := Arr{len: 1}
	a[0] = 69
	p := &a[0]
	assert *p == 69
}
