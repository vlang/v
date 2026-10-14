module io

import os

// ChunkReader serves its chunks one read at a time and then reports Eof, so
// combinators can be tested over short reads without a real stream.
struct ChunkReader {
mut:
	chunks [][]u8
}

fn (mut c ChunkReader) read(mut buf []u8) !int {
	if c.chunks.len == 0 {
		return Eof{}
	}
	chunk := c.chunks[0]
	c.chunks.delete(0)
	if buf.len < chunk.len {
		c.chunks.insert(0, chunk[buf.len..])
		return copy(mut buf, chunk[..buf.len])
	}
	return copy(mut buf, chunk)
}

fn drain(mut r Reader) []u8 {
	mut b := []u8{len: 64}
	mut n := 0
	for {
		got := r.read(mut b[n..]) or { break }
		if got == 0 {
			break
		}
		n += got
		if n == b.len {
			unsafe { b.grow_len(64) }
		}
	}
	return b[..n]
}

fn test_limited_reader() {
	mut cr := ChunkReader{
		chunks: ['abcdefghij'.bytes()]
	}
	mut l := limited_reader(cr, 4)
	assert drain(mut l) == 'abcd'.bytes()
	// exhausted: further reads report Eof
	mut b := []u8{len: 4}
	l.read(mut b) or {
		assert err is Eof
		return
	}
	assert false, 'exhausted limited reader must report Eof'
}

fn test_tee_reader_mirrors() {
	path := os.join_path(os.temp_dir(), 'v_io_tee_test.txt')
	mut w := os.open_file(path, 'w') or { panic('must open: ${err}') }
	defer {
		w.close()
		os.rm(path) or {}
	}
	cr := &ChunkReader{
		chunks: ['hi'.bytes()]
	}
	tr := tee_reader(cr, w)
	assert drain(mut Reader(tr)) == 'hi'.bytes()
	w.close()
	assert os.read_file(path) or { panic('must read: ${err}') } == 'hi'
}

fn test_multi_reader_concatenates() {
	mut c1 := &ChunkReader{
		chunks: ['ab'.bytes()]
	}
	mut c2 := &ChunkReader{
		chunks: ['cd'.bytes()]
	}
	mut m := multi_reader([Reader(c1), Reader(c2)])
	assert drain(mut Reader(m)) == 'abcd'.bytes()
	mut empty := multi_reader([]Reader{})
	mut eb := []u8{len: 4}
	empty.read(mut eb) or {
		assert err is Eof
		return
	}
	assert false, 'empty multi_reader must report Eof'
}

fn test_section_reader_window() {
	mut cr := ChunkReader{
		chunks: ['abcdefghij'.bytes()]
	}
	mut s := section_reader(cr, 5)
	assert drain(mut Reader(s)) == 'abcde'.bytes()
	mut b := []u8{len: 4}
	s.read(mut b) or {
		assert err is Eof
		return
	}
	assert false, 'section reader must stop at the window end'
}

fn test_read_byte_and_full() {
	mut c := ChunkReader{
		chunks: ['x'.bytes(), 'yz'.bytes()]
	}
	assert read_byte(mut Reader(c))! == `x`
	assert read_byte(mut Reader(c))! == `y`
	mut rest := []u8{len: 1}
	read_full(mut Reader(c), mut rest)!
	assert rest.bytestr() == 'z'
	read_byte(mut Reader(c)) or {
		assert err is Eof
		return
	}
	assert false, 'read_byte at Eof must error'
	read_full(mut Reader(c), mut rest) or {
		assert err is Eof
		return
	}
	assert false, 'read_full past Eof must error'
}

fn test_buffered_read_byte_and_unread() {
	mut cr := ChunkReader{
		chunks: ['hey'.bytes()]
	}
	mut br := new_buffered_reader(reader: cr, cap: 8)
	defer {
		br.free()
	}
	assert br.read_byte()! == `h`
	assert br.read_byte()! == `e`
	br.unread_byte()!
	assert br.read_byte()! == `e`
	assert br.read_byte()! == `y`
	br.read_byte() or {
		assert err is Eof
		return
	}
	assert false, 'buffered read_byte at Eof must error'
	br.unread_byte() or {
		return
	}
	assert false, 'unread with nothing read must error'
}
