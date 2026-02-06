module picoev

$if windows {
	#include <winsock2.h>
	#include <ws2tcpip.h>
} $else {
	#include <sys/select.h>
}

pub struct SelectLoop {
mut:
	id  int
	now i64
}

type LoopType = SelectLoop

// create_select_loop creates a new `SelectLoop` struct with the given `id`.
pub fn create_select_loop(id int) !&SelectLoop {
	return &SelectLoop{
		id: id
	}
}

// updates the events associated with a file descriptor in the event loop.
@[direct_array_access]
fn (mut pv Picoev) update_events(fd int, events int) int {
	// check if fd is in range
	assert fd < max_fds

	pv.file_descriptors[fd].events = u32(events & picoev_readwrite)
	return 0
}

// performs a single iteration of the select-based event loop.
@[direct_array_access]
fn (mut pv Picoev) poll_once(max_wait_in_sec int) int {
	// Initializes sets for read, write, and error events
	readfds, writefds, errorfds := C.fd_set{}, C.fd_set{}, C.fd_set{}

	// setup
	C.FD_ZERO(&readfds)
	C.FD_ZERO(&writefds)
	C.FD_ZERO(&errorfds)

	mut maxfd := 0

	// finds the maximum file descriptor and adds sockets to the sets `fd_sets`.
	for target in pv.file_descriptors {
		if target.loop_id == pv.loop.id {
			if target.events & picoev_read != 0 {
				C.FD_SET(target.fd, &readfds)
				if maxfd < target.fd {
					maxfd = target.fd
				}
			}
			if target.events & picoev_write != 0 {
				C.FD_SET(target.fd, &writefds)
				if maxfd < target.fd {
					maxfd = target.fd
				}
			}
		}
	}

	// select and handle sockets if any
	tv := C.timeval{
		tv_sec:  u64(max_wait_in_sec)
		tv_usec: 0
	}
	r := C.select(maxfd + 1, &readfds, &writefds, &errorfds, &tv)
	if r == -1 {
		// timeout
		return -1
	} else if r > 0 {
		// Iterates through file descriptors and calls their callbacks for triggered events
		for target in pv.file_descriptors {
			if target.loop_id == pv.loop.id {
				mut read_events := 0
				if C.FD_ISSET(target.fd, &readfds) != 0 {
					read_events |= picoev_read
				}
				if C.FD_ISSET(target.fd, &writefds) != 0 {
					read_events |= picoev_write
				}
				if read_events != 0 {
					trace_fd('do callback ${target.fd}')
					// do callback!
					unsafe { target.cb(target.fd, read_events, &pv) }
				}
			}
		}
	}
	return 0
}
