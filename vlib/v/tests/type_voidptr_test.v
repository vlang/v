@[unsafe]
fn memcpy(mut dest voidptr, src voidptr, len u32) voidptr {
	mut d := unsafe { &u8(dest) }
	s := unsafe { &u8(src) }
	mut l := len
	for l > 0 {
		l--
		unsafe {
			d[l] = s[l]
		}
	}
	return dest
}

fn test_mut_voidptr_arg() {
	mut a := [1, 2]!
	b := [3, 4]!
	mut aptr := voidptr(unsafe { &a[0] })
	returned := unsafe { memcpy(mut aptr, &b[0], sizeof(int)) }
	assert a == [3, 2]!
	assert returned == aptr
}

fn replace_byteptr(mut value byteptr, replacement byteptr) byteptr {
	value = replacement
	return value
}

fn replace_charptr(mut value charptr, replacement charptr) charptr {
	value = replacement
	return value
}

fn test_mut_builtin_pointer_args() {
	mut first := [u8(1), 2]!
	mut second := [u8(3), 4]!
	mut bytes := unsafe { byteptr(&first[0]) }
	next_bytes := unsafe { byteptr(&second[0]) }
	returned_bytes := replace_byteptr(mut bytes, next_bytes)
	assert bytes == next_bytes
	assert returned_bytes == next_bytes
	mut chars := unsafe { charptr(&first[0]) }
	next_chars := unsafe { charptr(&second[0]) }
	returned_chars := replace_charptr(mut chars, next_chars)
	assert chars == next_chars
	assert returned_chars == next_chars
}
