// A `[` on the line after a postfix `++`/`--` starts a new statement or
// expression, not an index of the incremented value.
fn postfix_newline_array_fails() ![]u8 {
	return error('fail')
}

fn test_postfix_inc_dec_before_array_in_or_block() {
	mut fails := 0
	a := postfix_newline_array_fails() or {
		fails++
		[]u8{}
	}
	mut left := 5
	b := postfix_newline_array_fails() or {
		left--
		[u8(1), 2]
	}
	assert a.len == 0
	assert fails == 1
	assert b == [u8(1), 2]
	assert left == 4
}

fn test_postfix_inc_before_array_in_if_branch_and_block() {
	mut n := 0
	arr := if n == 0 {
		n++
		[1, 2]
	} else {
		[3]
	}
	assert arr == [1, 2]
	{
		n++
		[1, 2].contains(1)
	}
	n--
	[3, 4].contains(3)
	assert n == 1
}
