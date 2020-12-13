// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// TODO Replace `u32` by `usize` once available

[direct_array_access]
[unsafe]
pub fn memcpy(mut dest voidptr, src voidptr, len u32) voidptr {
	mut d := byteptr(dest)
	s := byteptr(src)
	for l := u32(0); l < len; l++ {
		unsafe {
			d[l] = s[l]
		}
	}
	return dest
}

[direct_array_access]
[unsafe]
pub fn memmove(mut dest voidptr, src voidptr, len u32) voidptr {
	mut d := byteptr(dest)
	s := byteptr(src)
	if d < s {
		for l := u32(0); l < len; l++ {
			unsafe {
				d[l] = s[l]
			}
		}
	} else {
		mut l := len
		for l > 0 {
			l--
			unsafe {
				d[l] = s[l]
			}
		}
	}
	return dest
}

[direct_array_access]
pub fn memcmp(str1 voidptr, str2 voidptr, len u32) int {
	s1 := byteptr(str1)
	s2 := byteptr(str2)
	mut l := len
	for l > 0 {
		l--
		unsafe {
			if s1[l] != s2[l] {
				return if s1[l] < s2[l] { -1 } else { 1 }
			}
		}
	}
	return 0
}
