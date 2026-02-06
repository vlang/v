module io

fn test_multi_writer_write_successful() {
	w0 := TestWriter{}
	w1 := TestWriter{}
	mut mw := new_multi_writer(w0, w1)
	n := mw.write('0123456789'.bytes()) or {
		assert false
		return
	}
	assert n == 10
	assert w0.bytes == '0123456789'.bytes()
	assert w1.bytes == '0123456789'.bytes()
}

fn test_multi_writer_write_incomplete() {
	w0 := TestWriter{}
	w1 := TestIncompleteWriter{}
	mut mw := new_multi_writer(w0, w1)
	n := mw.write('0123456789'.bytes()) or {
		assert w0.bytes == '0123456789'.bytes()
		assert w1.bytes == '012345678'.bytes()
		return
	}
	assert false
}

fn test_multi_writer_write_error() {
	w0 := TestWriter{}
	w1 := TestErrorWriter{}
	w2 := TestWriter{}
	mut mw := new_multi_writer(w0, w1, w2)
	n := mw.write('0123456789'.bytes()) or {
		assert w0.bytes == '0123456789'.bytes()
		assert w2.bytes == []
		return
	}
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
