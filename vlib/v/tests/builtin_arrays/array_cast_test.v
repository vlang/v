fn test_array_cast() {
	mut keys := ['']
	unsafe {
		vp := voidptr(&keys)
		mut p := &[]string(vp)
		(*p)[0] = 'hi'
		assert *p == ['hi']
	}
	assert keys[0] == 'hi'
}

fn test_int() {
	mut arr := [2.3, 3]
	unsafe {
		vp := voidptr(&arr)
		p := &[]f64(vp)
		assert *p == arr
	}
}

fn test_ref_array_cast_to_elem_ptr_uses_array_data() {
	mut buf := []u8{len: 4}
	p := unsafe { &u8(&buf) }
	unsafe {
		p[0] = `A`
		p[1] = `B`
	}
	assert buf[0] == `A`
	assert buf[1] == `B`
}

struct Header {
mut:
	first  u8
	second u8
}

fn test_ref_array_cast_to_struct_ptr_uses_array_data() {
	mut buf := []u8{len: int(sizeof(Header))}
	header := unsafe { &Header(&buf) }
	unsafe {
		header.first = `x`
		header.second = `y`
	}
	assert buf[0] == `x`
	assert buf[1] == `y`
}

fn test_ref_array_cast_to_array_header_keeps_header_address() {
	mut arr := [1, 2]
	header := unsafe { &array(&arr) }
	assert unsafe { header.len } == 2
}
