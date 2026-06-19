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

// for issue 27457: an `if`/`match` expression returning a fixed array, where the
// branches mix a function call with a fixed array literal, generated invalid C
// (a struct passed where the `.ret_arr` member was expected, and vice versa).
fn issue_27457_fa() [3]int {
	return [1, 2, 3]!
}

fn issue_27457_ret_if(c bool) [3]int {
	return if c { issue_27457_fa() } else { [9, 9, 9]! }
}

fn issue_27457_ret_if_rev(c bool) [3]int {
	return if c { [9, 9, 9]! } else { issue_27457_fa() }
}

fn issue_27457_ret_if_chain(n int) [3]int {
	return if n == 0 {
		issue_27457_fa()
	} else if n == 1 {
		[5, 5, 5]!
	} else {
		[7, 7, 7]!
	}
}

fn issue_27457_ret_match(c bool) [3]int {
	return match c {
		true { issue_27457_fa() }
		else { [9, 9, 9]! }
	}
}

fn issue_27457_ret_match_multi(n int) [3]int {
	return match n {
		0 { issue_27457_fa() }
		1 { [5, 5, 5]! }
		else { [7, 7, 7]! }
	}
}

fn issue_27457_assign(c bool) [3]int {
	x := if c { issue_27457_fa() } else { [4, 4, 4]! }
	return x
}

fn test_fixed_array_return_from_mixed_if_match_branches() {
	assert issue_27457_ret_if(true) == [1, 2, 3]!
	assert issue_27457_ret_if(false) == [9, 9, 9]!

	assert issue_27457_ret_if_rev(true) == [9, 9, 9]!
	assert issue_27457_ret_if_rev(false) == [1, 2, 3]!

	assert issue_27457_ret_if_chain(0) == [1, 2, 3]!
	assert issue_27457_ret_if_chain(1) == [5, 5, 5]!
	assert issue_27457_ret_if_chain(2) == [7, 7, 7]!

	assert issue_27457_ret_match(true) == [1, 2, 3]!
	assert issue_27457_ret_match(false) == [9, 9, 9]!

	assert issue_27457_ret_match_multi(0) == [1, 2, 3]!
	assert issue_27457_ret_match_multi(1) == [5, 5, 5]!
	assert issue_27457_ret_match_multi(2) == [7, 7, 7]!

	assert issue_27457_assign(true) == [1, 2, 3]!
	assert issue_27457_assign(false) == [4, 4, 4]!
}
