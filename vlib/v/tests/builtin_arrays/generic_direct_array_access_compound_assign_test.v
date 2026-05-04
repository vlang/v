// Tests that @[direct_array_access] with compound assignment (+=) works
// correctly in generic functions. Previously, the element type was emitted
// as int_literal (8 bytes) instead of int (4 bytes) in the generated C,
// causing incorrect pointer arithmetic and corrupted array values.

@[heap]
struct Container[T] {
	shape []int
}

@[direct_array_access]
fn (c &Container[T]) cumsum[T]() []int {
	mut sizes := [0]
	sizes << [1, 1, 1]
	mut rt := 0
	for i in 0 .. sizes.len {
		tmp := rt
		rt += sizes[i]
		sizes[i] += tmp
	}
	return sizes
}

fn test_generic_direct_array_access_compound_assign() {
	c := Container[f64]{
		shape: [3, 3]
	}
	result := c.cumsum()
	assert result == [0, 1, 2, 3], 'expected [0, 1, 2, 3] got ${result}'
}

fn test_generic_direct_array_access_compound_assign_int() {
	c := Container[int]{
		shape: [3]
	}
	result := c.cumsum()
	assert result == [0, 1, 2, 3], 'expected [0, 1, 2, 3] got ${result}'
}
