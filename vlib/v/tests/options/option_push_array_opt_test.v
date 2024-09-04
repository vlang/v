fn test_dump_array_opt_push_string() {
	mut vals := []?string{cap: 4}
	vals << none
	vals << 'a'
	vals << 'b'
	vals << 'c'

	t := dump(vals[0])
	assert t == none

	t2 := vals[0]
	assert t2 == none

	assert vals.len == 4
}
