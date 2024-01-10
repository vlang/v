fn do_a_thing(i int) ?int {
	if i < 0 {
		return none
	}
	return i
}

fn test_match_error_to_none() {
	for i := -1; i < 1; i++ {
		if r := do_a_thing(i) {
			println(r)
		} else {
			match err {
				none {
					assert true
				}
				else {
					assert false
				}
			}
		}
	}
}
