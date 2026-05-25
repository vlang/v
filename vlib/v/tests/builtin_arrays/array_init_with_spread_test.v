const base_array = [
	1,
	2,
]

const base_strings = ['a', 'b']

struct Point {
	x int
	y int
}

const base_points = [Point{1, 2}, Point{3, 4}]

fn test_array_init_with_spread() {
	complete := [...base_array,
		3,
		4,
	]
	assert base_array == [1, 2]
	assert complete == [1, 2, 3, 4]
	assert complete.len == 4

	second := [...base_array, 5, 6, 7]
	assert second == [1, 2, 5, 6, 7]
}

fn test_array_init_with_only_spread() {
	mut copy := [...base_array]
	assert copy == [1, 2]
	copy << 99
	// spread produces an independent copy
	assert base_array == [1, 2]
	assert copy == [1, 2, 99]
}

fn test_array_spread_strings() {
	merged := [...base_strings, 'c', 'd']
	assert merged == ['a', 'b', 'c', 'd']
}

fn test_array_spread_structs() {
	pts := [...base_points, Point{5, 6}]
	assert pts.len == 3
	assert pts[0] == Point{1, 2}
	assert pts[1] == Point{3, 4}
	assert pts[2] == Point{5, 6}
}

fn test_array_spread_local_var() {
	v_arr := [7, 8, 9]
	x := [...v_arr, 100]
	assert v_arr == [7, 8, 9]
	assert x == [7, 8, 9, 100]
}

fn test_array_spread_chained() {
	first := [...base_array, 3]
	second := [...first, 4]
	assert second == [1, 2, 3, 4]
}

// Regression: `[...base]` should produce an independent deep copy. Modifying
// an inner []int after the spread must not affect the base.
fn test_array_spread_nested_arrays_are_deep_cloned() {
	mut base := [[1, 2], [3, 4]]
	mut copy := [...base]
	copy[0] << 99
	assert base[0] == [1, 2]
	assert copy[0] == [1, 2, 99]
}

// Regression: assigning into a nested element of the spread copy must not
// mutate the base. Catches shallow-copy regressions in any backend that
// reuses the base's inner storage.
fn test_array_spread_nested_element_assignment_isolated() {
	mut base := [[1, 2]]
	mut b := [...base]
	b[0][0] = 9
	assert base[0][0] == 1
	assert b[0][0] == 9
}

// Regression: indexing a spread literal inline (`[...base, 3][0]`) must keep
// the spread base in the AST instead of dropping it during parsing.
fn test_array_spread_indexed_inline() {
	assert [...base_array, 3][0] == 1
	assert [...base_array, 3][1] == 2
	assert [...base_array, 3][2] == 3
}

// Regression: appended string variables must be cloned, matching the
// behavior of the regular `[s1, s2]` array-literal path.
fn test_array_spread_appended_string_variable() {
	base := ['a', 'b']
	mut s := 'c'
	arr := [...base, s]
	s = 'mutated'
	assert arr == ['a', 'b', 'c']
}

type Ints = []int

// Regression: type-alias of an array should be accepted as a spread base.
fn test_array_spread_alias_base() {
	a := Ints([10, 20, 30])
	b := [...a, 40]
	assert b.len == 4
	assert b[0] == 10
	assert b[3] == 40
	c := [...a]
	assert c.len == 3
}
