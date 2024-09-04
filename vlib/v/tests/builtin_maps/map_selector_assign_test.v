struct Test {
mut:
	data int
}

fn test_map_selector_assign() {
	mut m := map[int]Test{}
	m[0].data = 1
	println(m[0].data)
	assert m[0].data == 1
}
