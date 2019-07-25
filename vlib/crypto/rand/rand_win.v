module rand

#flag -Llibraries/bcrypt -lbcrypt
#include <winternl.h>
// #include <ntstatus.h>
// #include <winerror.h>
#include <bcrypt.h>

import const (
	STATUS_SUCCESS
	BCRYPT_USE_SYSTEM_PREFERRED_RNG
)