module io

struct MemWriter2 {
mut:
	data []u8
}

fn (mut w MemWriter2) write(buf []u8) !int {
	w.data << buf
	return buf.len
}

fn test_write_string() {
	mut w := MemWriter2{}
	n := write_string(mut w, 'hello')!
	assert n == 5
	assert w.data.bytestr() == 'hello'
}
