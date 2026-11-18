module bytes

import io

fn write_all(mut w io.Writer, s string) !int {
	return w.write(s.bytes())!
}

fn test_buffer_write_read() {
	mut b := new_buffer()
	b.write_string('hello')
	assert b.str() == 'hello'
	assert b.len() == 5
	mut buf := []u8{len: 3}
	n := b.read(mut buf)!
	assert n == 3
	assert buf.bytestr() == 'hel'
	assert b.str() == 'lo'
	b.reset()
	assert b.len() == 0
}

fn test_buffer_read_reports_eof_when_empty() {
	mut b := new_buffer_with_bytes('ab'.bytes())
	mut buf := []u8{len: 1}
	assert b.read(mut buf)! == 1
	assert buf[0] == `a`
	assert b.read(mut buf)! == 1
	assert buf[0] == `b`
	b.read(mut buf) or {
		assert err is io.Eof
		return
	}
	assert false, 'expected io.Eof after Buffer is drained'
}

fn test_buffer_write_accepts_writer_param() {
	mut b := new_buffer()
	n := write_all(mut b, 'hello')!
	assert n == 5
	assert b.str() == 'hello'
}
