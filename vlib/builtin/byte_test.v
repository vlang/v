fn test_clone() {
	a := [byte(0), 1, 2]
	b := a.clone()
	assert b.len == 3
	assert b[0] == 0
	assert b[1] == 1
	assert b[2] == 2
	assert b[1].str() == '1'
	xx := byte(35)
	assert xx.str() == '35'
	assert xx.ascii_str() == '#'
	println(typeof(`A`).name)
	assert typeof(`A`).name == 'rune'
	x := rune(`A`)
	assert x.str() == 'A'
	assert typeof(x).name == 'rune'
	//
	y := `Z`
	assert typeof(y).name == 'rune'
	assert y.str() == 'Z'
	// assert b[1].str() == '1' TODO
}
