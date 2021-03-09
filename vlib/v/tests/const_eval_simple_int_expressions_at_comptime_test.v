const kb = 1024

const buf_siz = 1024 * kb

fn test_consts() {
	assert kb == 1024
	assert buf_siz == 1024 * kb
	assert buf_siz == 1048576
	println(buf_siz)
}

fn test_fixed_size_arrays_can_use_known_comptime_consts_as_their_size() {
	buf := [buf_siz]byte{}
	println(buf.len)
	assert buf.len == 1048576
}

// zbuf := [1024*1024]byte{}
// error: fixed size cannot be zero or negative

// buf := [1048576]byte{}
