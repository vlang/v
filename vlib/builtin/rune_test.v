fn test_repeat() {
	r1 := `V`
	r2 := `👋`

	assert r1.repeat(5) == 'VVVVV'
	assert r2.repeat(5) == '👋👋👋👋👋'

	assert r1.repeat(1) == r1.str()
	assert r2.repeat(1) == r2.str()

	assert r1.repeat(0) == ''
	assert r2.repeat(0) == ''
}
