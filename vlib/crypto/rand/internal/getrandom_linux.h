#ifndef V_CRYPTO_RAND_GETRANDOM_LINUX_H
#define V_CRYPTO_RAND_GETRANDOM_LINUX_H

#if defined(__linux__)

/*
 * v_crypto_getrandom fills `buf` with `n` random bytes, like getrandom(2) with no
 * flags. Using libc allows its platform-specific optimizations, including vDSO
 * support where available. If the compiler cannot find <sys/random.h>, use the
 * syscall as before. Both paths retain getrandom(2)'s return value and flags.
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

#endif /* __linux__ */

#endif
