fn test_assign() {
	_ = 123
}

fn fn_with_blank_param(_ int) {
	_ = 456
}

fn test_fn_with_blank_param() {
	fn_with_blank_param(321)
}

fn test_for_in_simple() {
	for _ in [1, 2, 3] {
		assert true
	}
}

fn test_for_in_key() {
	mut i := 1
	for _, v in [1, 2, 3] {
		println("v: $v")
		println("i: $i")
		assert v == i
		i++
	}
}

fn test_for_in_val() {
	mut j := 0
	for i, _ in [3, 4, 5] {
		println("i: $i")
		println("j: $j")
		assert i == j
		j++
	}
}

// TODO Make work on windows-msvc
fn test_for_in_both() {
	// Problem probably comes from
	// for (int _ = 0; _ < tmp1.len; _++)
	// int _ = tmp1.data[_]
	// => tmp variables
	for _, _ in [1, 2, 3] {
		assert true
	}
	println("for in both")
}

fn test_nested_ident() {
	for _ in [1, 2, 3] {
		for _ in [1, 2, 3] {
			assert true
		}
	}
	println("for in nested")
}
