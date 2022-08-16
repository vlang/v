// Copyright (c) 2022 John Lloyd. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#include <stdlib.h>

fn C.arc4random_buf(p &byte, n usize)

pub fn read(bytes_needed int) ?[]u8 {
	mut buffer := unsafe { malloc_noscan(bytes_needed) }
	C.arc4random_buf(buffer, bytes_needed)
	return unsafe { buffer.vbytes(bytes_needed) }
}
