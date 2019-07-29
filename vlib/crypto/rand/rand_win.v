module rand

#flag -Llibraries/bcrypt -lbcrypt
#include <winternl.h>
#include <bcrypt.h>

import const (
	STATUS_SUCCESS
	BCRYPT_USE_SYSTEM_PREFERRED_RNG
)

pub fn read(bytes_needed int) ?[]byte {	
	mut buffer := malloc(bytes_needed)
	// use BCRYPT_USE_SYSTEM_PREFERRED_RNG as we passed null as algo
	status := C.BCryptGenRandom(0, buffer, bytes_needed, BCRYPT_USE_SYSTEM_PREFERRED_RNG)

	if !C.NT_SUCCESS(status) {
		return error('crypt.random.read: error')
	}

	// NOTE: temp until we have []bytes(buff)
	return new_array_from_c_array_no_alloc_rand(bytes_needed, 1, 1, buffer)
}
