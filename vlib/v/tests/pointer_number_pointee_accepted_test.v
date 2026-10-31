// A number read through a pointer must be stored as the pointee type says (see
// vlib/v/checker/tests/pointer_number_pointee_mismatch_err.vv). These uses stay accepted.
fn read_u64(p &u64) u64 {
	return *p
}

fn read_u32(p &u32) u32 {
	return *p
}

fn read_i8(p &i8) i8 {
	return *p
}

fn read_char(p &char) u8 {
	return u8(*p)
}

fn read_any(p voidptr) u8 {
	return unsafe { *&u8(p) }
}

fn test_same_size_pointees() {
	signed := i32(-1)
	assert read_u32(&signed) == 0xffffffff
	byte_value := u8(0x80)
	assert read_i8(&byte_value) == -128
	assert read_char(&byte_value) == 0x80
}

fn test_voidptr_and_casts() {
	small := u8(9)
	assert read_any(&small) == 9
	mut words := [u64(0), 0]!
	bytes := unsafe { &u8(&words[0]) }
	unsafe {
		bytes[0] = 5
	}
	assert read_u64(&words[0]) == 5
	assert read_u64(unsafe { &u64(voidptr(&words[0])) }) == 5
}

@[unsafe]
fn test_unsafe_block() {
	wide := [u64(7), 0]!
	mut p := &u8(unsafe { nil })
	unsafe {
		p = &wide[0]
	}
	assert *p == 7
}
