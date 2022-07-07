module main

fn string_none() string|none {
	return none
}

fn test_sumtype_with_none() {
	x := string_none()
	res := match x {
		string {
			false
		}
		none {
			true
		}
	}
	assert res
}
