fn test_match_aliases() {
	a := u8(97)
	ret := match a {
		`0`...`9`, `a`...`f` { 'OK' }
		else { 'NOT OK' }
	}
	println(ret)
	assert ret == 'OK'
}
