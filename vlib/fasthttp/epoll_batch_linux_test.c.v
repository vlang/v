// vtest build: linux
module fasthttp

#include <sys/eventfd.h>

fn C.eventfd(initial u32, flags int) int

fn test_epoll_retains_ready_events_beyond_worker_batch() {
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer { C.close(epoll_fd) }
	mut fds := []int{}
	defer {
		for fd in fds { C.close(fd) }
	}
	for _ in 0 .. epoll_event_batch_size + 17 {
		fd := C.eventfd(1, 0)
		assert fd >= 0
		fds << fd
		assert add_fd_to_epoll(epoll_fd, fd, u32(C.EPOLLIN | C.EPOLLET)) == 0
	}
	mut events := [epoll_event_batch_size]C.epoll_event{}
	assert sizeof(events) == epoll_event_batch_size * sizeof(C.epoll_event)
	assert sizeof(events) <= 4096
	mut seen := map[int]bool{}
	for _ in 0 .. 4 {
		n := C.epoll_wait(epoll_fd, &events[0], events.len, 0)
		assert n >= 0
		if n == 0 { break }
		for i in 0 .. n {
			fd := unsafe { events[i].data.fd }
			assert fd in fds
			assert !seen[fd]
			seen[fd] = true
		}
	}
	assert seen.len == fds.len
	assert C.epoll_wait(epoll_fd, &events[0], events.len, 0) == 0
}
