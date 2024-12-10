// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#include <sys/random.h>

fn C.getentropy(buf voidptr, buflen usize) int

// read returns an array of `bytes_needed` random bytes read from the OS.
pub fn read(bytes_needed int) ![]u8 {
	mut buffer := []u8{len: bytes_needed}
	status := C.getentropy(buffer.data, bytes_needed)
	if status != 0 {
		return &ReadError{}
	}
	return buffer
}
