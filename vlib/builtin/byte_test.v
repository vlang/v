fn test_clone() {
	a := [byte(0), 1, 2]
	b := a.clone()
	assert b.len == 3
	assert b[0] == 0
	assert b[1] == 1
	assert b[2] == 2
	println(b[1].ascii_str())
	println(typeof(`A`))
	x := rune(`A`)
	assert x.str() == 'A'
	assert typeof(x) == 'rune'
	//
	y := `Z`
	assert typeof(y) == 'rune'
	assert y.str() == 'Z'
	// assert b[1].str() == '1' TODO
}
