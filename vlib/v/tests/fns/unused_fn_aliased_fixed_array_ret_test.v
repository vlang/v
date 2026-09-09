type V4 = [4]f32
type M4 = [4]V4

fn unused_make_matrix() M4 {
	mut c := M4{}
	c[0][0] = f32(1.234)
	return c
}

fn test_unused_fn_aliased_fixed_array_ret() {
	assert true
}
