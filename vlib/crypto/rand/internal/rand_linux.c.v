// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module internal

// A portable `-os cross` compiler snapshot is generated on Linux, so it bakes
// in this file even when its C source is later compiled on an Apple host.
$if macos || ios || openbsd {
	#include <sys/random.h>
} $else {
	#include <sys/syscall.h>
}

pub const C.SYS_getrandom int

fn C.getentropy(buf voidptr, buflen usize) i32

const read_batch_size = 256

// read fills `buffer` with random bytes from the OS.
pub fn read(mut buffer []u8) ! {
	mut bytes_read := 0
	mut remaining_bytes := buffer.len
	// getrandom syscall wont block if requesting <= 256 bytes
	for bytes_read < buffer.len {
		batch_size := if remaining_bytes > read_batch_size {
			read_batch_size
		} else {
			remaining_bytes
		}
		rbytes := unsafe { getrandom(batch_size, &u8(buffer.data) + bytes_read) }
		if rbytes == -1 {
			return &ReadError{}
		}
		bytes_read += rbytes
		remaining_bytes -= rbytes
	}
}

fn getrandom(bytes_needed int, buffer voidptr) int {
	if bytes_needed > read_batch_size {
		panic('getrandom() dont request more than ${read_batch_size} bytes at once.')
	}
	$if macos || ios || openbsd {
		if C.getentropy(buffer, usize(bytes_needed)) != 0 {
			return -1
		}
		return bytes_needed
	} $else {
		return unsafe { C.syscall(C.SYS_getrandom, buffer, bytes_needed, 0) }
	}
}
