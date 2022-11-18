// optional
pub fn foo1() bool {
	return false
}

pub fn bar1(i int) ?int {
	if i < 0 {
		return none
	}
	return if i == 0 {
		if foo1() {
			1
		} else {
			2
		}
	} else {
		3
	}
}

// result
pub fn foo2() bool {
	return false
}

pub fn bar2(i int) !int {
	if i < 0 {
		return error('')
	}
	return if i == 0 {
		if foo2() {
			1
		} else {
			2
		}
	} else {
		3
	}
}

fn test_if_expr_nested_with_optional_result() {
	ret11 := bar1(0) or { 0 }
	println(ret11)
	assert ret11 == 2

	ret12 := bar1(1) or { 0 }
	println(ret12)
	assert ret12 == 3

	ret21 := bar2(0) or { 0 }
	println(ret21)
	assert ret21 == 2

	ret22 := bar2(1) or { 0 }
	println(ret22)
	assert ret22 == 3
}
