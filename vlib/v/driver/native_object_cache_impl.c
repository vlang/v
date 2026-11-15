/* Shared by the SDK-backed header and standalone compiler snapshots. */
#ifndef V_NATIVE_OBJECT_CACHE_IMPL
#define V_NATIVE_OBJECT_CACHE_IMPL

#if defined(ENOSYS)
#define V_NATIVE_OBJECT_CACHE_ENOSYS ENOSYS
#elif defined(__linux__)
#define V_NATIVE_OBJECT_CACHE_ENOSYS 38
#elif defined(_WIN32)
#define V_NATIVE_OBJECT_CACHE_ENOSYS 40
#else
#define V_NATIVE_OBJECT_CACHE_ENOSYS 78
#endif

#if defined(__linux__)
#if defined(EOPNOTSUPP)
#define V_NATIVE_OBJECT_CACHE_EOPNOTSUPP EOPNOTSUPP
#else
#define V_NATIVE_OBJECT_CACHE_EOPNOTSUPP 95
#endif
/* Linux UAPI syscall tables: x86, arm, asm-generic, powerpc and s390.
 * Native headers remain authoritative when the SDK provides the number. */
#if defined(SYS_renameat2)
#define V_NATIVE_OBJECT_CACHE_RENAMEAT2 SYS_renameat2
#elif defined(__x86_64__) && defined(__ILP32__)
#define V_NATIVE_OBJECT_CACHE_RENAMEAT2 (0x40000000 + 316)
#elif defined(__x86_64__)
#define V_NATIVE_OBJECT_CACHE_RENAMEAT2 316
#elif defined(__i386__)
#define V_NATIVE_OBJECT_CACHE_RENAMEAT2 353
#elif defined(__arm__)
#define V_NATIVE_OBJECT_CACHE_RENAMEAT2 382
#elif defined(__aarch64__) || defined(__riscv) || defined(__loongarch__)
#define V_NATIVE_OBJECT_CACHE_RENAMEAT2 276
#elif defined(__powerpc__)
#define V_NATIVE_OBJECT_CACHE_RENAMEAT2 357
#elif defined(__s390__)
#define V_NATIVE_OBJECT_CACHE_RENAMEAT2 347
#endif
#endif

/* Use the syscall: Bionic's renameat2 wrapper is unavailable before API 30.
 * Keep these guards in C so a portable vc snapshot can build on other hosts. */
static inline int v_native_object_cache_rename_noreplace(const char *source, const char *destination) {
#if defined(__linux__) && defined(V_NATIVE_OBJECT_CACHE_RENAMEAT2)
	/* Linux AT_FDCWD is -100 and RENAME_NOREPLACE is 1. Avoid importing fcntl
	 * types into headerless snapshots or requiring the flag in old headers. */
	if (syscall(V_NATIVE_OBJECT_CACHE_RENAMEAT2, -100, source, -100, destination, 1U) == 0) {
		return 0;
	}
	return errno;
#else
	(void)source;
	(void)destination;
	return V_NATIVE_OBJECT_CACHE_ENOSYS;
#endif
}

static inline int v_native_object_cache_publish(const char *source, const char *destination) {
#ifndef _WIN32
#ifdef V_NATIVE_OBJECT_CACHE_HEADERLESS
	if (link((char *)source, (char *)destination) == 0) {
#else
	if (link(source, destination) == 0) {
#endif
		return 0;
	}
	const int code = errno;
#if defined(__linux__)
	/* Android SELinux can forbid links in app-private storage; some filesystems
	 * do not implement hard links. Neither prevents an atomic no-replace rename. */
	if (code == EACCES || code == EPERM || code == V_NATIVE_OBJECT_CACHE_EOPNOTSUPP) {
		return v_native_object_cache_rename_noreplace(source, destination);
	}
#endif
	return code;
#else
	(void)source;
	(void)destination;
	return V_NATIVE_OBJECT_CACHE_ENOSYS;
#endif
}

#endif
