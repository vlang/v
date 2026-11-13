#ifndef V_OS_FD_POLL_H
#define V_OS_FD_POLL_H

#if __V_HEADERLESS_LIBC
// The standalone preamble already owns libc types. Keep system headers out of
// this path, and use a private poll descriptor with the native int-sized fd.
struct v_os_pollfd {
	int fd;
	short events;
	short revents;
};
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__) || defined(__ANDROID__)
typedef unsigned int v_os_nfds_t;
#else
typedef size_t v_os_nfds_t;
#endif
#ifdef __APPLE__
int v_os_poll_sys(struct v_os_pollfd*, v_os_nfds_t, int) __asm__("_poll");
#else
int v_os_poll_sys(struct v_os_pollfd*, v_os_nfds_t, int) __asm__("poll");
#endif
// Including this header makes os's C bindings header-owned.
int select(int, fd_set*, fd_set*, fd_set*, struct timeval*);

static inline short v_os_fd_poll_revents(int fd, short events) {
	struct v_os_pollfd descriptor = {fd, events, 0};
	if (v_os_poll_sys(&descriptor, 1, 0) <= 0) {
		return 0;
	}
	return descriptor.revents;
}
#else
// Native pollfd is owned by the system header; emitting a V C struct for it
// collides with poll.h during the vc snapshot bootstrap on Unix targets.
#if defined(__linux__) && !defined(__ANDROID__)
// Cross-target tests may have only a partial target SDK. Probe before using
// glibc's feature header; compilers without __has_include keep the old path.
#if !defined(__has_include)
#include <features.h>
#elif __has_include(<features.h>)
#include <features.h>
#endif
#endif
#if defined(__linux__) && defined(__GLIBC__)
#include <sys/poll.h>
#else
#include <poll.h>
#endif

static inline short v_os_fd_poll_revents(int fd, short events) {
	struct pollfd descriptor = {fd, events, 0};
	if (poll(&descriptor, 1, 0) <= 0) {
		return 0;
	}
	return descriptor.revents;
}

#endif

#endif
