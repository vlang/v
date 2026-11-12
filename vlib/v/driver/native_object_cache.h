#ifndef V_NATIVE_OBJECT_CACHE_H
#define V_NATIVE_OBJECT_CACHE_H

#include <errno.h>

#ifndef _WIN32
#if (defined(__V_HEADERLESS_LIBC) && __V_HEADERLESS_LIBC) || defined(CUSTOM_DEFINE_v3_headerless_bootstrap)
/* Match the libc declarations in headerless compiler snapshots. Including
 * unistd.h/fcntl.h here would redefine their types and structures. */
#define V_NATIVE_OBJECT_CACHE_HEADERLESS 1
int link(char *, char *);
#if defined(__linux__)
long syscall(long, ...);
#endif
#else
#include <fcntl.h>
#include <unistd.h>
#endif
#endif

#if defined(__linux__)
#include <sys/syscall.h>
#endif

/* Use the syscall: Bionic's renameat2 wrapper is unavailable before API 30.
 * Keep these guards in C so a portable vc snapshot can build on other hosts. */
static inline int v_native_object_cache_rename_noreplace(const char *source, const char *destination) {
#if defined(__linux__) && defined(SYS_renameat2)
	/* Linux AT_FDCWD is -100 and RENAME_NOREPLACE is 1. Avoid importing fcntl
	 * types into headerless snapshots or requiring the flag in old headers. */
	if (syscall(SYS_renameat2, -100, source, -100, destination, 1U) == 0) {
		return 0;
	}
	return errno;
#else
	(void)source;
	(void)destination;
	return ENOSYS;
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
	if (code == EACCES || code == EPERM || code == EOPNOTSUPP) {
		return v_native_object_cache_rename_noreplace(source, destination);
	}
#endif
	return code;
#else
	(void)source;
	(void)destination;
	return ENOSYS;
#endif
}

#endif
