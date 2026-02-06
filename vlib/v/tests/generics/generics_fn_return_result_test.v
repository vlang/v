fn make_2x3[T](mut res [][]T) ! {
	mut a := []T{len: 6}
	res = reshape[T](a, [2, 3])!
}

fn reshape[T](y []T, dims []int) ![][]T {
	mut res := [][]T{len: dims[0], init: []T{len: 1}}
	return res
}

fn test_generic_fn_return_result() {
	mut acopy := [][]f32{len: 1, init: []f32{len: 1}}
	make_2x3(mut acopy)!
	dump(acopy)
	assert '${acopy}' == '[[0.0], [0.0]]'

	mut bcopy := [][]u8{len: 1, init: []u8{len: 1}}
	make_2x3(mut bcopy)!
	dump(bcopy)
	assert '${bcopy}' == '[[0], [0]]'
}
