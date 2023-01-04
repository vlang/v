struct Abc {}

type Test = Abc | bool | int

fn test(a Test) Test {
	return match a {
		Abc {
			20
		}
		int {
			match a {
				1 { true }
				2 { false }
				else { -1 }
			}
		}
		bool {
			1
		}
	}
}

fn test_nested_match_expr() {
	println(test(1))
	assert test(1) == Test(true)

	println(test(2))
	assert test(2) == Test(false)

	println(test(3))
	assert test(3) == Test(-1)

	println(test(true))
	assert test(true) == Test(1)
}
