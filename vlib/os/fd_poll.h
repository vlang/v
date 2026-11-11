#ifndef V_OS_FD_POLL_H
#define V_OS_FD_POLL_H

// Native pollfd is owned by the system header; emitting a V C struct for it
// collides with poll.h during the vc snapshot bootstrap on Unix targets.
#if defined(__linux__) && !defined(__ANDROID__)
#include <features.h>
#endif
#if defined(__linux__) && defined(__GLIBC__)
#include <sys/poll.h>
#else
#include <poll.h>
#endif
#if defined(__APPLE__)
#include <sys/select.h>
#endif

static inline short v_os_fd_poll_revents(int fd, short events) {
	struct pollfd descriptor = {fd, events, 0};
	if (poll(&descriptor, 1, 0) <= 0) {
		return 0;
	}
	return descriptor.revents;
}

#endif
