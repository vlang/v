// Copyright (c) 2022 John Lloyd. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module internal

// NOTE(vlibc): deliberately left on libc, even under -d vlibc. OpenBSD
// exposes randomness through the arc4random libc API, and no verified raw
// getrandom/getentropy syscall number is wired in openbsd_bare, so there
// is no pure-V syscall path to use here.

#include <stdlib.h>

fn C.arc4random_buf(p &u8, n uint)

// read fills `buffer` with random bytes from the OS.
pub fn read(mut buffer []u8) ! {
	C.arc4random_buf(buffer.data, buffer.len)
}
