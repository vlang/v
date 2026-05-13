module main

struct DirLike {
	d_name [256]u8
}

fn main() {
	mut d := DirLike{}
	d.d_name[255] = 7
	assert d.d_name[255] == 7
	assert d.d_name.len == 256
	println('ok')
}
