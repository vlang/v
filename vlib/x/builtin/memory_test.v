import x.builtin

struct Color {
	r byte
	g byte
	b byte
}

fn (c &Color) eq(o &Color) bool {
	return c.r == o.r && c.g == o.g && c.b == o.b
}

fn test_memcpy() {
	mut arr_1a := [1, 2]!!
	arr_1b := [3, 4]!!
	unsafe {
		builtin.memcpy(mut arr_1a, arr_1b, sizeof(int))
	}
	assert arr_1a == [3, 2]!!
	mut arr_2a := [1, 2]!!
	arr_2b := [3, 4]!!
	unsafe {
		builtin.memcpy(mut arr_2a, arr_2b, sizeof(arr_2a))
	}
	assert arr_2a == arr_2b
	mut c_1a := Color{0, 0, 0}
	c_1b := Color{255, 255, 255}
	unsafe {
		builtin.memcpy(mut c_1a, c_1b, sizeof(byte) * 2)
	}
	assert c_1a.eq(Color{255, 255, 0})
	mut c_2a := Color{0, 0, 0}
	c_2b := Color{255, 255, 255}
	unsafe {
		builtin.memcpy(mut c_2a, c_2b, sizeof(Color))
	}
	assert c_2a.eq(c_2b)
	mut c_3a := Color{0, 0, 0}
	c_3b := Color{255, 255, 255}
	unsafe {
		builtin.memcpy(mut c_3a, c_3b, sizeof(c_2a))
	}
	assert c_3a.eq(c_3b)
	// TODO Tests for edge cases compared to memmove
}

fn test_memmove() {
	mut arr_1a := [1, 2]!!
	arr_1b := [3, 4]!!
	unsafe {
		builtin.memmove(mut arr_1a, arr_1b, sizeof(int))
	}
	assert arr_1a == [3, 2]!!
	arr_2b := [3, 4]!!
	mut arr_2a := [1, 2]!!
	unsafe {
		builtin.memmove(mut arr_2a, arr_2b, sizeof(arr_2a))
	}
	assert arr_2a == arr_2b
	mut c_1a := Color{0, 0, 0}
	c_1b := Color{255, 255, 255}
	unsafe {
		builtin.memmove(mut c_1a, c_1b, sizeof(byte) * 2)
	}
	assert c_1a.eq(Color{255, 255, 0})
	c_2b := Color{255, 255, 255}
	mut c_2a := Color{0, 0, 0}
	unsafe {
		builtin.memmove(mut c_2a, c_2b, sizeof(Color))
	}
	assert c_2a.eq(c_2b)
	mut c_3a := Color{0, 0, 0}
	c_3b := Color{255, 255, 255}
	unsafe {
		builtin.memmove(mut c_3a, c_3b, sizeof(c_2a))
	}
	assert c_3a.eq(c_3b)
	// TODO Tests for edge cases compared to memcpy
}

fn test_memcmp() {
	i_a := 1
	i_b := 1
	assert builtin.memcmp(i_a, i_b, sizeof(int)) == 0
	i_c := 2
	assert builtin.memcmp(i_a, i_c, sizeof(int)) == -1
}
