module bytes

fn test_buffer_write_read() {
	mut b := new_buffer()
	b.write_string('hello')
	assert b.str() == 'hello'
	assert b.len() == 5
	mut buf := []u8{len: 3}
	n := b.read(mut buf)
	assert n == 3
	assert buf.bytestr() == 'hel'
	assert b.str() == 'lo'
	b.reset()
	assert b.len() == 0
}
