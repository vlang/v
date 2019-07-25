module rand

	// if atomic.CompareAndSwapInt32(&r.used, 0, 1) {
	// 	// First use of randomness. Start timer to warn about
	// 	// being blocked on entropy not being available.
	// 	t := time.AfterFunc(60*time.Second, warnBlocked)
	// 	defer t.Stop()
	// }
	// r.mu.Lock()
	// if r.prov == 0 {
	// 	const provType = syscall.PROV_RSA_FULL
	// 	const flags = syscall.CRYPT_VERIFYCONTEXT | syscall.CRYPT_SILENT
	// 	err := syscall.CryptAcquireContext(&r.prov, nil, nil, provType, flags)
	// 	if err != nil {
	// 		r.mu.Unlock()
	// 		return 0, os.NewSyscallError("CryptAcquireContext", err)
	// 	}
	// }
	// r.mu.Unlock()

	// if len(b) == 0 {
	// 	return 0, nil
	// }
	// err = syscall.CryptGenRandom(r.prov, uint32(len(b)), &b[0])
	// if err != nil {
	// 	return 0, os.NewSyscallError("CryptGenRandom", err)
	// }
	// return len(b), nil

#include <winternl.h>
#include <ntstatus.h>
#include <winerror.h>
#include <stdio.h>
#include <sal.h>
#include <bcrypt.h>


import const (
	// BCRYPT_RNG_ALGORITHM
	// BCRYPT_OBJECT_LENGTH
	// MS_PRIMITIVE_PROVIDER
	STATUS_SUCCESS
	BCRYPT_USE_SYSTEM_PREFERRED_RNG
)

fn new_array_from_c_array_rand(len, cap, elm_size int, c_array voidptr) array {
	arr := array {
		len: len
		cap: cap
		element_size: elm_size
		data: malloc(cap * elm_size)
	}
	// TODO Write all memory functions (like memcpy) in V
	C.memcpy(arr.data, c_array, len * elm_size)
	return arr
}

pub fn read(bytes_needed int) []byte {	
	// buffer := [bytes_needed]byte
	buffer := malloc(bytes_needed)
	// bufer := [512]byte

	status := C.BCryptGenRandom(
		0,                               // Alg Handle pointer; NUll is passed as BCRYPT_USE_SYSTEM_PREFERRED_RNG flag is used
		buffer,                          // Address of the buffer that recieves the random number(s)
		bytes_needed,                    // Size of the buffer in bytes
		BCRYPT_USE_SYSTEM_PREFERRED_RNG)

	if !C.NT_SUCCESS(status) {
		panic('crypt.random.read: error')
	}

	// mut d := []byte
	// for b in buffer {
	// 	d << b
	// }
	return []byte
	// return new_array_from_c_array_rand(bytes_needed, bytes_needed, bytes_needed, buffer)
}

// pub fn read(bytes_needed int)
// 	bcripd_genrand_handle := voidptr()
// 	// buffer = rand_pool_add_begin(pool, bytes_needed);
// 	buffer := [bytes_needed]byte
// 	for buffer {
// 		size_t bytes = 0;
// 		if (BCryptGenRandom(NULL, buffer, bytes_needed,
// 							BCRYPT_USE_SYSTEM_PREFERRED_RNG) == STATUS_SUCCESS)
// 			bytes = bytes_needed;

// 		rand_pool_add_end(pool, bytes, 8 * bytes);
// 		entropy_available = rand_pool_entropy_available(pool);
// 	}
// 	if (entropy_available > 0)
// 		return entropy_availabl

// )




// unsigned char *rand_pool_add_begin(RAND_POOL *pool, size_t len)
// {
//     if (len == 0)
//         return NULL;

//     if (len > pool->max_len - pool->len) {
//         RANDerr(RAND_F_RAND_POOL_ADD_BEGIN, RAND_R_RANDOM_POOL_OVERFLOW);
//         return NULL;
//     }

//     if (pool->buffer == NULL) {
//         RANDerr(RAND_F_RAND_POOL_ADD_BEGIN, ERR_R_INTERNAL_ERROR);
//         return NULL;
//     }

//     if (!rand_pool_grow(pool, len))
//         return NULL;
//     return pool->buffer + pool->len;
// }