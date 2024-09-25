type Buffer = []u8

pub fn (mut sb Buffer) vbytes() string {
	return sb[0..sb.len].a().str()
}

pub fn (mut sb Buffer) a() Buffer {
	return sb
}

fn test_main() {
	mut b := Buffer([]u8{cap: 10})
	b << 1
	b << 2
	assert b.vbytes() == 'Buffer([1, 2])'
}
