fn test_match_in_map_or_expr() {
	a := 5
	some_map := {
		3: '3'
		4: '4'
	}
	something := match a {
		7 { '7' }
		6 { '6' }
		5 { '5' }
		else { some_map[a] or { a.str() } } // here is the error
	}
	println(something)
	assert something == '5'
}
