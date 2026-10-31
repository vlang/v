import io

struct ReadBuf {
mut:
	data []u8
	pos  int
}

fn (mut r ReadBuf) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	n := copy(mut buf, r.data[r.pos..])
	r.pos += n
	return n
}

struct WriteBuf {
mut:
	data []u8
}

fn (mut w WriteBuf) write(buf []u8) !int {
	if buf.len == 0 {
		return error('empty buffer')
	}
	w.data << buf
	return buf.len
}

fn test_make_readerwriter_reads_what_the_reader_returns() {
	mut reader := ReadBuf{
		data: 'hello world'.bytes()
	}
	mut rw := io.make_readerwriter(&reader, &WriteBuf{})
	mut buf := []u8{len: 5}
	n := rw.read(mut buf) or { panic(err) }
	assert n == 5
	assert buf[0] == `h`
	assert buf[4] == `o`
	assert reader.pos == 5
}

fn test_make_readerwriter_writes_what_the_writer_accepts() {
	mut writer := &WriteBuf{}
	mut rw := io.make_readerwriter(ReadBuf{}, writer)
	n := rw.write('abcdef'.bytes()) or { panic(err) }
	assert n == 6
	assert writer.data == 'abcdef'.bytes()
}

fn test_make_readerwriter_propagates_the_readers_eof() {
	mut rw := io.make_readerwriter(ReadBuf{}, &WriteBuf{})
	mut buf := []u8{len: 4}
	mut saw_eof := false
	rw.read(mut buf) or {
		assert err is io.Eof
		saw_eof = true
	}
	assert saw_eof, 'expected the reader to report eof'
}

fn test_make_readerwriter_propagates_the_writers_error() {
	mut writer := &WriteBuf{}
	mut rw := io.make_readerwriter(ReadBuf{}, writer)
	mut saw_error := false
	rw.write([]u8{}) or {
		assert err.msg() == 'empty buffer'
		saw_error = true
	}
	assert saw_error, 'expected the writer to reject an empty buffer'
	assert writer.data == []u8{}
}

// A readerwriter can copy bytes between its two halves, which is what `io.cp`
// uses it for.
fn test_make_readerwriter_copies_through_both_halves() {
	mut reader := ReadBuf{
		data: 'abcdefghij'.bytes()
	}
	mut writer := &WriteBuf{}
	mut rw := io.make_readerwriter(&reader, writer)
	mut buf := []u8{len: 4}
	for _ in 0 .. 2 {
		read := rw.read(mut buf) or { panic(err) }
		written := rw.write(buf[..read]) or { panic(err) }
		assert written == read
	}
	assert writer.data == 'abcdefgh'.bytes()
	assert reader.pos == 8
}
