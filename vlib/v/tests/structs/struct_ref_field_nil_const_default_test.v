const null_ptr = unsafe { nil }

struct RefDefaultWindow {
	w int
}

struct RefDefaultCtx {
mut:
	window &RefDefaultWindow = null_ptr
	voidp  voidptr           = null_ptr
	bytep  &u8               = null_ptr
	charp  &char             = null_ptr
}

fn test_ref_field_with_nil_const_default() {
	c := RefDefaultCtx{}
	assert c.window == unsafe { nil }
	assert c.voidp == unsafe { nil }
	assert c.bytep == unsafe { nil }
	assert c.charp == unsafe { nil }
}
