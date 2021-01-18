// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#include <Security/SecRandom.h>

#flag darwin -framework Security

fn C.SecRandomCopyBytes() int

pub fn read(bytes_needed int) ?[]byte {
	mut buffer := malloc(bytes_needed)
	status := C.SecRandomCopyBytes(0, bytes_needed, buffer)
	if status != 0 {
		return read_error
	}
	return c_array_to_bytes_tmp(bytes_needed, buffer)
}
