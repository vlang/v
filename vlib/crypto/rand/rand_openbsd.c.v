// Copyright (c) 2022 John Lloyd. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#include <stdlib.h>

fn C.arc4random_buf(p &u8, n usize)

// read fills `buffer` with random bytes from the OS.
pub fn read(mut buffer []u8) ! {
	C.arc4random_buf(buffer.data, buffer.len)
}
