// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#include <Security/SecRandom.h>

#flag darwin -framework Security

fn C.SecRandomCopyBytes(rnd C.SecRandomRef, count usize, bytes voidptr) int

// read returns an array of `bytes_needed` random bytes read from the OS.
pub fn read(bytes_needed int) ![]u8 {
	mut buffer := []u8{len: bytes_needed}
	status := C.SecRandomCopyBytes(C.SecRandomRef(0), bytes_needed, buffer.data)
	if status != 0 {
		return &ReadError{}
	}
	return buffer
}
