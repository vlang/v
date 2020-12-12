import x.builtin

fn test_strlen() {
	bytes1 := [byte(`a`), `b`, `c`, 0x00]!!
	unsafe {
		assert builtin.strlen(bytes1) == 3
	}
	mut bytes2 := [10]byte{}
	bytes2[0] = `a`
	bytes2[1] = `b`
	bytes2[2] = 0x00
	unsafe {
		assert builtin.strlen(bytes2) == 2
	}
	c_str := c'test'
	unsafe {
		assert builtin.strlen(c_str) == 4
	}
	raw_byteptr := byteptr(c'anothertest')
	unsafe {
		assert builtin.strlen(raw_byteptr) == 11
	}
}
