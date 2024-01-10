struct LeakStruct {
mut:
	some_bytes []u8
}

fn (mut l LeakStruct) free() {
	unsafe {
		l.some_bytes.free()
	}
}

fn main() {
	z := &LeakStruct{
		some_bytes: []u8{len: 1000}
	}
	println(z.some_bytes.len)
}
