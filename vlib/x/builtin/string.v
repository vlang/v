// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// TODO Replace `u32` by `usize` once available

// TODO Make it a method of byteptr ? It would easier to apply it to voidptr and charptr
[unsafe]
pub fn strlen(str byteptr) u32 {
	l := u32(0)
	unsafe {
		for str[l] != `\0` {
			l++
		}
	}
	return l
}
