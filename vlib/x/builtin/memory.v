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
	mut l := len
	for l > 0 {
		l--
		unsafe {
			d[l] = s[l]
		}
	}
	return dest
}
