[unsafe]
fn memcpy(mut dest voidptr, src voidptr, len u32) voidptr {
	mut d := byteptr(dest)
	s := byteptr(src)
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
	unsafe { memcpy(mut &a[0], &b[0], sizeof(int)) }
	assert a == [3, 2]!
}
