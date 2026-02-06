const start = 1
const start_2 = 4
const end = 3
const end_2 = 8
//
const start_rune = `a`
const start_2_rune = `d`
const end_rune = `c`
const end_2_rune = `i`
//
const start_cast_expr = u16(1)
const end_cast_expr = u16(5)

fn test_match_int_const_ranges() {
	mut results := []int{}
	for x in 0 .. 10 {
		match x {
			start...end { results << 1 }
			start_2...5 { results << 2 }
			6...end_2 { results << 3 }
			else { results << 4 }
		}
	}
	assert results == [4, 1, 1, 1, 2, 2, 3, 3, 3, 4]
}

fn test_match_rune_const_ranges() {
	mut results := []int{}
	for x in `a` .. `l` {
		match x {
			start_rune...end_rune { results << 1 }
			start_2_rune...`e` { results << 2 }
			`f`...end_2_rune { results << 3 }
			else { results << 4 }
		}
	}
	assert results == [1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4]
}

fn test_match_expr_int_const_ranges() {
	mut results := []int{}
	for x in 0 .. 10 {
		result := match x {
			start...end { 1 }
			start_2...5 { 2 }
			6...end_2 { 3 }
			else { 4 }
		}
		results << result
	}
	assert results == [4, 1, 1, 1, 2, 2, 3, 3, 3, 4]
}

fn test_match_expr_rune_const_ranges() {
	mut results := []int{}
	for x in `a` .. `l` {
		result := match x {
			start_rune...end_rune { 1 }
			start_2_rune...`e` { 2 }
			`f`...end_2_rune { 3 }
			else { 4 }
		}
		results << result
	}
	assert results == [1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4]
}

fn test_match_expr_integer_cast_const_ranges() {
	c := u16(3)
	match c {
		start_cast_expr...end_cast_expr {
			assert c == u16(3)
		}
		else {}
	}
}
