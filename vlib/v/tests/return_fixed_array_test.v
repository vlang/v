type Abc = [3]int

fn return_fixed_array() [3]int {
	return [1, 2, 3]!
}

fn return_fixed_array_in_multi_return() ([3]int, [3]int) {
	return [1, 2, 3]!, [4, 5, 6]!
}

fn return_fixed_array_with_alias() Abc {
	return [1, 2, 3]!
}

fn test_with_alias() {
	a := return_fixed_array_with_alias()
	assert a == [1, 2, 3]!
}

fn test_without_alias() {
	a := return_fixed_array()
	assert a == [1, 2, 3]!
}

// for issue 20366: returns mut fixed array
fn returns_mut_fixed_array(mut fixed_array [3]int) [3]int {
	return fixed_array
}

fn test_returns_mut_fixed_array() {
	mut fixed := [59, 101, 200]!
	res := returns_mut_fixed_array(mut fixed)
	assert res == [59, 101, 200]!
}

// for issue 20373: returns option / result fixed array
pub fn returns_option_fixed_array(fixed [3]int) ?[3]int {
	return fixed
}

pub fn returns_result_fixed_array(fixed [3]int) ![3]int {
	return fixed
}

fn test_returns_option_and_result_fixed_array() {
	mut fixed := [int(59), 101, 200]!

	mut res := returns_option_fixed_array(fixed) or {
		assert false
		return
	}
	assert res == [59, 101, 200]!

	res = returns_result_fixed_array(fixed) or {
		assert false
		return
	}
	assert res == [59, 101, 200]!
}

fn issue_27345_hud_rgba() [4]u8 {
	return [u8(1), 2, 3, 4]!
}

fn issue_27345_shop_category_color() [4]u8 {
	col := issue_27345_hud_rgba()
	return col
}

fn test_fixed_array_return_via_local_from_call() {
	assert issue_27345_shop_category_color() == [u8(1), 2, 3, 4]!
}

enum FixedArrayReturnMatchMedal {
	one
	two
	three
	four
	five
	six
}

fn issue_fixed_array_return_match_rgba(r u8, g u8, b u8, a u8) [4]u8 {
	return [r, g, b, a]!
}

fn issue_fixed_array_return_from_enum_match(medal FixedArrayReturnMatchMedal) [4]u8 {
	return match medal {
		.one { issue_fixed_array_return_match_rgba(1, 2, 3, 4) }
		.two { issue_fixed_array_return_match_rgba(5, 6, 7, 8) }
		.three { issue_fixed_array_return_match_rgba(9, 10, 11, 12) }
		.four { issue_fixed_array_return_match_rgba(13, 14, 15, 16) }
		.five { issue_fixed_array_return_match_rgba(17, 18, 19, 20) }
		.six { issue_fixed_array_return_match_rgba(21, 22, 23, 24) }
	}
}

fn issue_fixed_array_literal_return_from_enum_match(medal FixedArrayReturnMatchMedal) [4]u8 {
	return match medal {
		.one { [u8(1), 2, 3, 4]! }
		.two { [u8(5), 6, 7, 8]! }
		.three { [u8(9), 10, 11, 12]! }
		.four { [u8(13), 14, 15, 16]! }
		.five { [u8(17), 18, 19, 20]! }
		.six { [u8(21), 22, 23, 24]! }
	}
}

fn test_fixed_array_return_from_large_enum_match_call_branches() {
	assert issue_fixed_array_return_from_enum_match(.two) == [u8(5), 6, 7, 8]!
	assert issue_fixed_array_return_from_enum_match(.six) == [u8(21), 22, 23, 24]!
	assert issue_fixed_array_literal_return_from_enum_match(.two) == [u8(5), 6, 7, 8]!
	assert issue_fixed_array_literal_return_from_enum_match(.six) == [u8(21), 22, 23, 24]!
}
