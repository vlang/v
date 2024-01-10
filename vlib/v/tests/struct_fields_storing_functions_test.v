type Async_cb = fn (x []u8, mut y []u8) int

fn async_cb(b []u8, mut res []u8) int {
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
	buf := [u8(1), 2, 3]
	mut res := []u8{}
	res << 0x88
	async_cb(buf[0..2], mut res)
	data := Ep_arg{
		sfd: 1234
		cb: async_cb
	}
	data.cb(buf[1..2], mut res)
	res << 0x99
	eprintln(res)
	assert res == [u8(0x88), 0x01, 0x02, 0x02, 0x99]
}
