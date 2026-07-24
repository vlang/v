import crypto.rand

fn assert_random_bytes(bytes []u8) {
	mut histogram := [256]int{}
	for b in bytes {
		histogram[b]++
	}
	for h in histogram {
		assert h < 10
	}
}

fn test_bytes() {
	bytes := rand.bytes(32)!
	assert bytes.len == 32
	assert_random_bytes(bytes)
}

fn test_bytes_rejects_negative_length() {
	if _ := rand.bytes(-1) {
		assert false, 'expected rand.bytes(-1) to fail'
	} else {
		assert err.msg() == 'can not read < 0 random bytes'
	}
}

fn test_read() {
	mut buffer := []u8{len: 32}
	rand.read(mut buffer)!
	assert buffer.len == 32
	assert_random_bytes(buffer)
}

fn test_read_large_buffer() {
	mut buffer := []u8{len: 512}
	rand.read(mut buffer)!
	assert buffer[..256] != []u8{len: 256}
	assert buffer[256..] != []u8{len: 256}
}
