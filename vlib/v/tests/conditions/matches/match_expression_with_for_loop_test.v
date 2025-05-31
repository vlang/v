fn test_match_with_for_in_loop() {
	a := 101
	b := match a {
		101 {
			mut aa := []int{}
			for i in 0 .. 3 {
				aa << i
			}
			aa
		}
		else {
			[0]
		}
	}
	println(b)
	assert b == [0, 1, 2]
}

fn test_match_with_for_c_loop() {
	a := 101
	b := match a {
		101 {
			mut aa := []int{}
			for i := 0; i < 3; i++ {
				aa << i
			}
			aa
		}
		else {
			[0]
		}
	}
	println(b)
	assert b == [0, 1, 2]
}

fn test_match_with_for_loop() {
	a := 101
	b := match a {
		101 {
			mut aa := []int{}
			mut i := 0
			for {
				aa << i
				i++
				if i == 3 {
					break
				}
			}
			aa
		}
		else {
			[0]
		}
	}
	println(b)
	assert b == [0, 1, 2]
}
