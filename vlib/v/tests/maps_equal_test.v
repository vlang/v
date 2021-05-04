fn test_string_int() {
	mut m := map{
		'hi': 4
	}
	m2 := map{
		'hi': 5
	}
	assert m != m2
	m['hi']++
	assert m == m2
	m.delete('hi')
	assert m != m2
}
