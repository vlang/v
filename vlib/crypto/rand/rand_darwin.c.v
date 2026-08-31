// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#include <sys/random.h>

fn C.getentropy(buf voidptr, buflen usize) i32

const read_batch_size = 256

// read fills `buffer` with random bytes from the OS.
pub fn read(mut buffer []u8) ! {
	mut bytes_read := 0
	mut remaining_bytes := buffer.len
	for bytes_read < buffer.len {
		batch_size := if remaining_bytes > read_batch_size {
			read_batch_size
		} else {
			remaining_bytes
		}
		status := unsafe { C.getentropy(&u8(buffer.data) + bytes_read, batch_size) }
		if status != 0 {
			return &ReadError{}
		}
		bytes_read += batch_size
		remaining_bytes -= batch_size
	}
}
