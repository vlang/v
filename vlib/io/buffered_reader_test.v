module io

import rand
import arrays

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
	for i, _ in res {
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
	for i, _ in p {
		assert data[i] == p[i]
	}

	mut read := []u8{len: 2}
	br.read(mut read)!

	p = br.peek(4)!
	for i, _ in p {
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
	for i, _ in res {
		assert data[i] == res[i]
	}
}

fn test_peek_refill_buffer() {
	data := rand.bytes(16)!
	mut br := new_array_buffered_reader(data, 6)
	mut p := br.peek(4)!
	for i, _ in p {
		assert data[i] == p[i]
	}

	mut read := []u8{len: 4}
	br.read(mut read)!

	p = br.peek(4)!
	for i, _ in p {
		assert data[i + 4] == p[i]
	}
}

fn test_peek_reaches_eof() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data, 6)
	mut res := []u8{len: 4}
	br.read(mut res)!

	p := br.peek(4)! // offset now at 4, buffer has 2 bytes, need to read source for more
	assert p.len == 4

	r := br.read(mut res)!
	assert r == 4
	br.read(mut res) or { assert err is Eof }
}

fn test_peek_too_many_bytes() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data)
	mut p := br.peek(16)!
	assert p.len == 8
	for i, _ in p {
		assert data[i] == p[i]
	}
}

fn test_peek_repeated() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data)
	for j := 0; j < 8; j++ {
		mut p := br.peek(6)!
		assert p.len == 6
		for i, _ in p {
			assert data[i] == p[i]
		}
	}

	mut res := []u8{len: 8}
	r := br.read(mut res)!
	assert r == 8
	for i, _ in res {
		assert data[i] == res[i]
	}
}

fn test_peek_zero_and_negative() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data)
	p := br.peek(0)!
	assert p.len == 0
	br.peek(-1) or { assert true }
}

fn test_peek_does_not_advance_total_read() {
	data := rand.bytes(8)!
	mut br := new_array_buffered_reader(data)
	br.peek(4)!
	assert br.total_read == 0
	mut res := []u8{len: 4}
	br.read(mut res)!
	assert br.total_read == 4
	br.peek(4)!
	assert br.total_read == 4
}

struct OneByteReader {
	a []u8
mut:
	i int
}

fn new_one_byte_reader(a []u8) &OneByteReader {
	return &OneByteReader{
		a: a
	}
}

fn new_one_byte_buffered_reader(a []u8, cap ?int) &BufferedReader {
	r := new_one_byte_reader(a)
	return new_buffered_reader(reader: r)
}

fn (mut r OneByteReader) read(mut buf []u8) !int {
	if r.i == r.a.len {
		return Eof{}
	}
	read := copy(mut buf, [r.a[r.i]])
	r.i++
	return read
}

fn test_peek_zero_bytes() {
	data := rand.bytes(4)!
	mut br := new_one_byte_buffered_reader(data)
	mut p := br.peek(0)!
	assert p.len == 0

	p = br.peek(0)!
	assert p.len == 0

	p = br.peek(1)!
	assert p.len == 1

	p = br.peek(0)!
	assert p.len == 0
}

fn test_read_refills_buffer() {
	data := 'abc'.bytes()
	mut br := new_one_byte_buffered_reader(data)
	mut res := []u8{len: 4}
	// read refills the empty buffer from the underlying reader with a single
	// fill, then returns what that fill produced. The OneByteReader yields one
	// byte per read, so read returns 1 byte here, not the whole input: per the
	// Reader interface, read returns *up to* buf.len bytes and callers loop.
	read := br.read(mut res)!
	assert read == 1
	for i := 0; i < read; i++ {
		assert data[i] == res[i]
	}
}

fn test_peek_refills_buffer() {
	data := 'abc'.bytes()
	mut br := new_one_byte_buffered_reader(data)
	p := br.peek(4)!
	assert p.len == 3

	mut res := []u8{len: 4}
	read := br.read(mut res)!
	assert read == data.len
	for i := 0; i < read; i++ {
		assert data[i] == res[i]
	}
}

fn test_read_handles_eof_with_unread_data() {
	data := rand.bytes(8)!
	mut br := new_one_byte_buffered_reader(data, 16)
	mut p := br.peek(10)!
	assert p.len == 8

	p = br.peek(8)!
	assert p.len == 8

	mut res := []u8{len: 10}
	mut read := br.read(mut res)!
	assert read == 8

	br.read(mut res) or { assert err is Eof }
}

fn test_read_line_handles_eof_with_unread_data() {
	// Keep the payload delimiter-free; random bytes can contain `\n` and make
	// read_line correctly return a shorter first line.
	b := 'abcdefgh'.bytes()
	data := arrays.concat(b, `\n`)
	mut br := new_one_byte_buffered_reader(data, 16)
	mut p := br.peek(10)!
	assert p.len == 9

	p = br.peek(9)!
	assert p.len == 9

	line := br.read_line()!
	assert line.len == 8 // read_line return doesn't include \n

	br.read_line() or { assert err is Eof }
}

// https://github.com/vlang/v/pull/27928#issuecomment-5079703057
struct EofAfterDataReader {
	data []u8
mut:
	offset int
}

fn (mut r EofAfterDataReader) read(mut buf []u8) !int {
	if r.offset >= r.data.len {
		return Eof{}
	}
	n := copy(mut buf, r.data[r.offset..])
	r.offset += n
	return n
}

fn test_peek_does_not_return_stale_bytes() {
	mut source := &EofAfterDataReader{
		data: 'abc'.bytes()
	}
	mut reader := new_buffered_reader(reader: source, cap: 4)
	assert reader.peek(4)! == 'abc'.bytes()
}
