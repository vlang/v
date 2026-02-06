fn fails(i int) !(int, ?int) {
	return error('fails')
}

fn test_main() {
	a2, b2 := fails(2) or { 22, 22 }
	c2 := b2? as int
	assert b2 != none
	assert a2 == c2
}
