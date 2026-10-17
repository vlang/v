struct First {}

struct Second {}

type Node = First | Second

fn lower_first(_ First) !int {
	return 1
}

fn lower_second(_ Second) !int {
	return 2
}

fn cast_unsafe_match(node Node) !i64 {
	return i64(unsafe {
		match node {
			First { lower_first(node)! }
			Second { lower_second(node)! }
		}
	})
}

fn cast_unsafe_match_in_if_guard(node ?Node) !i64 {
	result := if value := node {
		i64(unsafe {
			match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		})
	} else {
		i64(0)
	}
	return result
}

// A cast of an `unsafe` block whose value is a `match` with propagating arms must lower
// the arms as values.
fn test_cast_of_unsafe_match_with_propagating_arms() {
	assert cast_unsafe_match(First{}) or { panic(err) } == 1
	assert cast_unsafe_match(Second{}) or { panic(err) } == 2
	assert cast_unsafe_match_in_if_guard(Second{}) or { panic(err) } == 2
	assert cast_unsafe_match_in_if_guard(none) or { panic(err) } == 0
}
