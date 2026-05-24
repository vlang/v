// Regression test for https://github.com/vlang/v/issues/27146
// Short struct init syntax (positional) inside an array literal in a `for in` loop.
struct Int {
	val int
}

struct Pair {
	a int
	b int
}

fn test_short_struct_init_in_for_in_array() {
	mut got := []int{}
	for a in [Int{1}, Int{2}, Int{3}] {
		got << a.val
	}
	assert got == [1, 2, 3]
}

fn test_short_struct_init_multi_fields_in_for_in_array() {
	mut got := []int{}
	for p in [Pair{1, 2}, Pair{3, 4}] {
		got << p.a
		got << p.b
	}
	assert got == [1, 2, 3, 4]
}

fn test_mixed_struct_init_styles_in_for_in_array() {
	mut got := []int{}
	for x in [Int{}, Int{5}, Int{ val: 6 }] {
		got << x.val
	}
	assert got == [0, 5, 6]
}
