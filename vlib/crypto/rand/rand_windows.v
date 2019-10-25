// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#flag windows -Llibraries/bcrypt
#flag windows -lbcrypt
#include <bcrypt.h>

const (
	STATUS_SUCCESS                  = 0x00000000
	BCRYPT_USE_SYSTEM_PREFERRED_RNG = 0x00000002
)

pub fn read(bytes_needed int) ?[]byte {	
	mut buffer := malloc(bytes_needed)
	// use BCRYPT_USE_SYSTEM_PREFERRED_RNG because we passed null as algo
	status := C.BCryptGenRandom(0, buffer, bytes_needed, BCRYPT_USE_SYSTEM_PREFERRED_RNG)
	if status != STATUS_SUCCESS {
		return read_error
	}
	return c_array_to_bytes_tmp(bytes_needed, buffer)
}
