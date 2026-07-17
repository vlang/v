type Addr = [32]u8

struct Holder {
	p &Addr = unsafe { nil }
}

fn takes(p &Addr) u8 {
	return unsafe { p[0] }
}

// https://github.com/vlang/v/issues/27794
fn test_inline_ref_init_of_fixed_array_alias_as_arg() {
	assert takes(&Addr{}) == 0
}

fn test_ref_init_of_fixed_array_alias_assigned() {
	a := &Addr{}
	assert takes(a) == 0
}

fn test_ref_init_of_fixed_array_alias_in_struct_field() {
	h := Holder{
		p: &Addr{}
	}
	assert takes(h.p) == 0
}

fn ret_ptr() &Addr {
	return &Addr{}
}

fn test_ref_init_of_fixed_array_alias_returned() {
	assert takes(ret_ptr()) == 0
}
