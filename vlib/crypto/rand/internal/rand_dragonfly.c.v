// Copyright (c) 2022 John Lloyd. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module internal

// Under -d vlibc there is no libc, so the arc4random_buf path is
// unavailable; use the raw getrandom syscall instead (SYS_getrandom 550,
// sys/sys/syscall.h) through builtin's vlibc_sys_getrandom helper.
$if !vlibc ? {
	#include <stdlib.h>

	fn C.arc4random_buf(p &u8, n uint)
}

const read_batch_size = 256

// read fills `buffer` with random bytes from the OS.
pub fn read(mut buffer []u8) ! {
	$if vlibc ? {
		mut bytes_read := 0
		mut remaining_bytes := buffer.len
		// getrandom will not block when requesting <= 256 bytes at once.
		for bytes_read < buffer.len {
			batch_size := if remaining_bytes > read_batch_size {
				read_batch_size
			} else {
				remaining_bytes
			}
			rbytes := unsafe { v_getrandom(batch_size, &u8(buffer.data) + bytes_read) }
			if rbytes == -1 {
				return &ReadError{}
			}
			bytes_read += rbytes
			remaining_bytes -= rbytes
		}
	} $else {
		C.arc4random_buf(buffer.data, buffer.len)
	}
}

$if vlibc ? {
	fn v_getrandom(bytes_needed int, buffer voidptr) int {
		if bytes_needed > read_batch_size {
			panic('getrandom() dont request more than ${read_batch_size} bytes at once.')
		}
		r := vlibc_sys_getrandom(buffer, uint(bytes_needed), 0)
		if r < 0 {
			return -1
		}
		return int(r)
	}
}
