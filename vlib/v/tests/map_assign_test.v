fn test_map_assign() {
	mut a := {'foo': 1}
	mut b := a
	a['foo'] = 100
	println(a)
	println(b)
	assert a == {'foo': 100}
	assert b == {'foo': 1}
}
