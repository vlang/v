// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

#flag windows -Llibraries/bcrypt
#flag windows -lbcrypt
#include <bcrypt.h>

const status_success = 0x00000000
const bcrypt_use_system_preferred_rng = 0x00000002

// read fills `buffer` with random bytes from the OS.
pub fn read(mut buffer []u8) ! {
	// use bcrypt_use_system_preferred_rng because we passed null as algo
	status := C.BCryptGenRandom(0, buffer.data, buffer.len, bcrypt_use_system_preferred_rng)
	if status != status_success {
		return &ReadError{}
	}
}
