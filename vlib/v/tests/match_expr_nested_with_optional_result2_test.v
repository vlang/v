// optional
fn foo1() ?int {
	return 1
}

fn bar1(i int) bool {
	return true
}

fn is_ok1(y int, m int) ?bool {
	return match true {
		y == 0 {
			false
		}
		y > 0 {
			match m {
				1 { bar1(foo1()?) }
				2 { false }
				else { none }
			}
		}
		else {
			none
		}
	}
}

// result
fn foo2() !int {
	return 1
}

fn bar2(i int) bool {
	return true
}

fn is_ok2(y int, m int) !bool {
	return match true {
		y == 0 {
			false
		}
		y > 0 {
			match m {
				1 { bar2(foo2()!) }
				2 { false }
				else { error('') }
			}
		}
		else {
			error('')
		}
	}
}

fn test_match_expr_nested_with_optional_result() {
	ret1 := is_ok1(2, 1) or { false }
	println(ret1)
	assert ret1

	ret2 := is_ok2(2, 1) or { false }
	println(ret2)
	assert ret2
}
