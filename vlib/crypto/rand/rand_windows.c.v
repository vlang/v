// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#flag windows -Llibraries/bcrypt
#flag windows -lbcrypt
#include <bcrypt.h>

const (
	status_success                  = 0x00000000
	bcrypt_use_system_preferred_rng = 0x00000002
)

pub fn read(bytes_needed int) ?[]byte {
	mut buffer := malloc(bytes_needed)
	// use bcrypt_use_system_preferred_rng because we passed null as algo
	status := C.BCryptGenRandom(0, buffer, bytes_needed, bcrypt_use_system_preferred_rng)
	if status != status_success {
		return read_error
	}
	return c_array_to_bytes_tmp(bytes_needed, buffer)
}
