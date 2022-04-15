// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#include <sys/random.h>

fn C.getrandom(p &byte, n usize, flags u32) int

const (
	read_batch_size = 256
)

// read returns an array of `bytes_needed` random bytes read from the OS.
pub fn read(bytes_needed int) ?[]u8 {
	mut buffer := unsafe { malloc_noscan(bytes_needed) }
	mut bytes_read := 0
	mut remaining_bytes := bytes_needed
	// getrandom syscall wont block if requesting <= 256 bytes
	for bytes_read < bytes_needed {
		batch_size := if remaining_bytes > rand.read_batch_size {
			rand.read_batch_size
		} else {
			remaining_bytes
		}
		rbytes := unsafe { getrandom(batch_size, buffer + bytes_read) }
		if rbytes == -1 {
			unsafe { free(buffer) }
			return IError(&ReadError{})
		}
		bytes_read += rbytes
	}
	return unsafe { buffer.vbytes(bytes_needed) }
}

fn v_getrandom(bytes_needed int, buffer voidptr) int {
	if bytes_needed > rand.read_batch_size {
		panic('getrandom() dont request more than $rand.read_batch_size bytes at once.')
	}
	return C.getrandom(buffer, bytes_needed, 0)
}
