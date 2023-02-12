// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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

// read returns an array of `bytes_needed` random bytes read from the OS.
pub fn read(bytes_needed int) ![]u8 {
	mut buffer := []u8{len: bytes_needed}
	// use bcrypt_use_system_preferred_rng because we passed null as algo
	status := C.BCryptGenRandom(0, buffer.data, bytes_needed, rand.bcrypt_use_system_preferred_rng)
	if status != rand.status_success {
		return &ReadError{}
	}
	return buffer
}
