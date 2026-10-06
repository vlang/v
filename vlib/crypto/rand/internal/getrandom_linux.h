#ifndef V_CRYPTO_RAND_GETRANDOM_LINUX_H
#define V_CRYPTO_RAND_GETRANDOM_LINUX_H

/*
 * v_crypto_getrandom fills `buf` with `n` random bytes, like getrandom(2) with no
 * flags. The libc function answers from the vDSO when it can (glibc 2.41 and later
 * on Linux 6.11 and later), without entering the kernel for every call; it is about
 * 13 times faster than the syscall for 16 bytes. A libc without <sys/random.h>
 * (glibc before 2.25, musl before 1.1.20) gets the syscall, as before.
 */
#include <unistd.h>
#include <sys/syscall.h>

#if defined(__has_include)
#if __has_include(<sys/random.h>)
#include <sys/random.h>
#define V_CRYPTO_RAND_LIBC_GETRANDOM 1
#endif
#endif

static inline long v_crypto_getrandom(void* buf, size_t n) {
#ifdef V_CRYPTO_RAND_LIBC_GETRANDOM
	return (long)getrandom(buf, n, 0);
#else
	return syscall(SYS_getrandom, buf, n, 0);
#endif
}

#endif
