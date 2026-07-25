module io

import rand

struct ArrayReader {
	a []u8
mut:
	i int
}

fn new_array_reader(a []u8) &ArrayReader {
	return &ArrayReader{
		a: a
	}
}

fn (mut r ArrayReader) read(mut buf []u8) !int {
	read := copy(mut buf, r.a[r.i..])
	r.i += read
	return read
}

fn new_array_buffered_reader(a []u8, cap ?int) &BufferedReader {
	r := new_array_reader(a)
	if c := cap {
		return new_buffered_reader(reader: r, cap: c)
	}
	return new_buffered_reader(reader: r)
}

fn test_read_basic() {
	data := rand.bytes(16)!
	mut br := new_array_buffered_reader(data)
	mut res := []u8{len: 16}
	r := br.read(mut res)!
	assert r == 16
	for i, byte in res {
		assert data[i] == res[i]
	}
}

fn test_empty() {
	data := []u8{}
	mut br := new_array_buffered_reader(data)
	mut res := []u8{len: 16}
	br.read(mut res) or { assert err is Eof }
}

fn test_peek_basic() {
	data := rand.bytes(16)!
	mut br := new_array_buffered_reader(data)
	mut p := br.peek(4)!
	for i, byte in p {
		assert data[i] == p[i]
	}

	mut read := []u8{len: 2}
	br.read(mut read)!

	p = br.peek(4)!
	for i, byte in p {
		assert data[i + 2] == p[i]
	}
}

fn test_peek_does_not_advance_offset() {
	data := rand.bytes(16)!
	mut br := new_array_buffered_reader(data)
	p := br.peek(8)!
	mut res := []u8{len: 8}
	r := br.read(mut res)!
	assert r == 8
	for i, byte in res {
		assert data[i] == res[i]
	}
}

fn test_peek_refill_buffer() {
	data := rand.bytes(16)!
	mut br := new_array_buffered_reader(data, 6)
	mut p := br.peek(4)!
	for i, byte in p {
		assert data[i] == p[i]
	}

	mut read := []u8{len: 4}
	br.read(mut read)!

	p = br.peek(4)!
	for i, byte in p {
		assert data[i + 4] == p[i]
	}
}

fn test_peek_reaches_eof() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data, 6)
	mut res := []u8{len: 4}
	br.read(mut res)!

	p := br.peek(4)!

	r := br.read(mut res)!
	assert r == 4
	br.read(mut res) or { assert err is Eof }
}

fn test_peek_too_many_bytes() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data)
	mut p := br.peek(16)!
	assert p.len == 8
	for i, byte in p {
		assert data[i] == p[i]
	}
}

fn test_peek_repeated() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data)
	for j := 0; j < 8; j++ {
		mut p := br.peek(6)!
		assert p.len == 6
		for i, byte in p {
			assert data[i] == p[i]
		}
	}

	mut res := []u8{len: 8}
	r := br.read(mut res)!
	assert r == 8
	for i, byte in res {
		assert data[i] == res[i]
	}
}

fn test_peek_zero_and_negative() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data, none)
	p := br.peek(0)!
	assert p.len == 0
	br.peek(-1) or { assert true }
}

fn test_peek_does_not_advance_total_read() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data, none)
	br.peek(4)!
	assert br.total_read == 0
	mut res := []u8{len: 4}
	br.read(mut res)!
	assert br.total_read == 4
	br.peek(4)!
	assert br.total_read == 4
}
