// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module internal

#include <errno.h>
#include <sys/syscall.h>
#insert "@VEXEROOT/vlib/crypto/rand/internal/getrandom_linux.h"

fn C.v_crypto_getrandom(buf voidptr, n uint) i64

const read_batch_size = 256

// read fills `buffer` with random bytes from the OS.
// Android requires kernel getrandom support (Linux 3.17 or a backport), not the
// API-28 bionic wrapper. Unavailable or denied syscalls return an error.
pub fn read(mut buffer []u8) ! {
	mut bytes_read := 0
	for bytes_read < buffer.len {
		batch_size := if buffer.len - bytes_read > read_batch_size {
			read_batch_size
		} else {
			buffer.len - bytes_read
		}
		// The pointer stays within the live buffer; the syscall retains no pointer.
		rbytes := unsafe {
			int(C.v_crypto_getrandom(&u8(buffer.data) + bytes_read, uint(batch_size)))
		}
		if rbytes < 0 && C.errno == C.EINTR {
			continue
		}
		if rbytes <= 0 {
			return &ReadError{}
		}
		bytes_read += rbytes
	}
}
