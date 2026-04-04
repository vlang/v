module io

fn test_multi_writer_write_successful() {
	mut mw := new_multi_writer(&TestWriter{}, &TestWriter{})
	n := mw.write('0123456789'.bytes()) or {
		assert false
		return
	}
	assert n == 10
}

fn test_multi_writer_write_incomplete() {
	mut mw := new_multi_writer(&TestWriter{}, &TestIncompleteWriter{})
	_ := mw.write('0123456789'.bytes()) or { return }
	assert false
}

fn test_multi_writer_write_error() {
	mut mw := new_multi_writer(&TestWriter{}, &TestErrorWriter{}, &TestWriter{})
	_ := mw.write('0123456789'.bytes()) or { return }
	assert false
}

struct TestWriter {
pub mut:
	bytes []u8
}

fn (mut w TestWriter) write(buf []u8) !int {
	w.bytes << buf
	return buf.len
}

struct TestIncompleteWriter {
pub mut:
	bytes []u8
}

fn (mut w TestIncompleteWriter) write(buf []u8) !int {
	b := buf[..buf.len - 1]
	w.bytes << b
	return b.len
}

struct TestErrorWriter {}

fn (mut w TestErrorWriter) write(buf []u8) !int {
	return error('error writer errored')
}
