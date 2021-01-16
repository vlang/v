type Async_cb = fn (x []byte, mut y []byte) int

fn async_cb(b []byte, mut res []byte) int {
	if b.len > 0 {
		res << b
	}
	return 0
}

struct Ep_arg {
mut:
	sfd int
	cb  Async_cb
}

fn test_struct_fn_field_can_be_used_directly() {
	buf := [byte(1), 2, 3]
	mut res := []byte{}
	res << 0x88
	async_cb(buf[0..2], mut res)
	data := Ep_arg{
		sfd: 1234
		cb: async_cb
	}
	data.cb(buf[1..2], mut res)
	res << 0x99
	eprintln(res)
	assert res == [byte(0x88), 0x01, 0x02, 0x02, 0x99]
}
