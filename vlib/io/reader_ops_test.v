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
	if _ := read_byte(mut Reader(c)) {
		assert false, 'read_byte at Eof must error'
	} else {
		assert err is Eof
	}
	if _ := read_full(mut Reader(c), mut rest) {
		assert false, 'read_full past Eof must error'
	} else {
		assert err is Eof
	}
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
	if _ := br.read_byte() {
		assert false, 'buffered read_byte at Eof must error'
	} else {
		assert err is Eof
	}
	if _ := br.unread_byte() {
		assert false, 'unread after failed read must error'
	} else {
		assert err.msg().contains('no byte to unread')
	}
}

struct OpsCountReader {
	count int
}

fn (mut r OpsCountReader) read(mut _buf []u8) !int {
	return r.count
}

struct OpsShortWriter {}

fn (mut w OpsShortWriter) write(buf []u8) !int {
	return buf.len - 1
}

struct OpsRecoveringReader {
mut:
	step int
}

fn (mut r OpsRecoveringReader) read(mut buf []u8) !int {
	r.step++
	if r.step == 1 { return error('recoverable reader failure') }
	if r.step == 2 { return 0 }
	if r.step == 3 {
		buf[0] = `a`
		return 1
	}
	return Eof{}
}

fn test_reader_combinators_preserve_buffer_length() {
	mut limited := limited_reader(&ChunkReader{ chunks: ['abcd'.bytes()] }, 2)
	mut section := section_reader(&ChunkReader{ chunks: ['abcd'.bytes()] }, 2)
	mut buf := []u8{len: 8, init: `x`}
	assert limited.read(mut buf)! == 2
	assert buf.len == 8
	assert buf.bytestr() == 'abxxxxxx'
	assert section.read(mut buf)! == 2
	assert buf.len == 8
	assert buf.bytestr() == 'abxxxxxx'
}

fn test_multi_reader_propagates_errors_and_preserves_zero_reads() {
	mut multi := multi_reader([Reader(&OpsRecoveringReader{}),
		Reader(&ChunkReader{ chunks: ['b'.bytes()] })])
	mut buf := []u8{len: 4}
	if _ := multi.read(mut buf) {
		assert false, 'non-EOF reader errors must surface'
	} else {
		assert err.msg() == 'recoverable reader failure'
	}
	assert multi.read(mut buf)! == 0
	assert multi.read(mut buf)! == 1
	assert buf[0] == `a`
	assert multi.read(mut buf)! == 1
	assert buf[0] == `b`
}

fn test_tee_reader_rejects_short_writes() {
	mut tee := tee_reader(&ChunkReader{ chunks: ['abc'.bytes()] }, &OpsShortWriter{})
	mut buf := []u8{len: 4}
	if _ := tee.read(mut buf) {
		assert false, 'a short write must surface'
	} else {
		assert err.msg().contains('short or invalid write')
	}
}

fn test_reader_combinators_reject_invalid_counts() {
	for count in [-1, 5] {
		mut buf := []u8{len: 4}
		mut limited := limited_reader(&OpsCountReader{ count: count }, 2)
		mut section := section_reader(&OpsCountReader{ count: count }, 2)
		mut multi := multi_reader([Reader(&OpsCountReader{ count: count })])
		mut tee := tee_reader(&OpsCountReader{ count: count }, &OpsShortWriter{})
		mut readers := [Reader(limited), Reader(section), Reader(multi), Reader(tee)]
		for mut reader in readers {
			if _ := reader.read(mut buf) {
				assert false
			} else {
				assert err.msg() == 'io: invalid read count'
			}
		}
		mut reader := Reader(&OpsCountReader{ count: count })
		if _ := read_byte(mut reader) {
			assert false
		} else {
			assert err.msg() == 'io: invalid read count'
		}
		if _ := read_full(mut reader, mut buf) {
			assert false
		} else {
			assert err.msg() == 'io: invalid read count'
		}
	}
	mut zero := Reader(&OpsCountReader{})
	mut buf := []u8{len: 1}
	if _ := read_full(mut zero, mut buf) {
		assert false
	} else {
		assert err.msg() == 'io: read_full: no progress'
	}
}

fn test_buffered_unread_byte_tracks_last_operation() {
	mut br := new_buffered_reader(reader: &ChunkReader{ chunks: ['abcdef'.bytes()] }, cap: 8)
	defer { br.free() }
	assert br.read_byte()! == `a`
	assert br.total_read == 1
	br.unread_byte()!
	assert br.total_read == 0
	if _ := br.unread_byte() {
		assert false
	} else {
		assert err.msg().contains('no byte')
	}
	assert br.read_byte()! == `a`
	mut buf := []u8{len: 1}
	assert br.read(mut buf)! == 1
	if _ := br.unread_byte() {
		assert false
	} else {
		assert err.msg().contains('no byte')
	}
	assert br.read_byte()! == `c`
	assert br.peek(1)! == [u8(`d`)]
	if _ := br.unread_byte() {
		assert false
	} else {
		assert err.msg().contains('no byte')
	}
	assert br.read_byte()! == `d`
	assert br.read_line()! == 'ef'
	if _ := br.unread_byte() {
		assert false
	} else {
		assert err.msg().contains('no byte')
	}
}
