module freestanding_define

const f = $embed_file(@FILE).len

pub fn hi() {
	println('hi from a_d_freestanding.c.v')
	println('f.len: ${f}')
}
