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

// create_select_loop creates a `SelectLoop` struct with `id`
pub fn create_select_loop(id int) !&SelectLoop {
	return &SelectLoop{
		id: id
	}
}

[direct_array_access]
fn (mut pv Picoev) update_events(fd int, events int) int {
	// check if fd is in range
	assert fd < max_fds

	pv.file_descriptors[fd].events = u32(events & picoev_readwrite)
	return 0
}

[direct_array_access]
fn (mut pv Picoev) poll_once(max_wait int) int {
	readfds, writefds, errorfds := C.fd_set{}, C.fd_set{}, C.fd_set{}

	// setup
	C.FD_ZERO(&readfds)
	C.FD_ZERO(&writefds)
	C.FD_ZERO(&errorfds)

	mut maxfd := 0

	// find the maximum socket for `select` and add sockets to the fd_sets
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
		tv_sec: u64(max_wait)
		tv_usec: 0
	}
	r := C.@select(maxfd + 1, &readfds, &writefds, &errorfds, &tv)
	if r == -1 {
		// timeout
		return -1
	} else if r > 0 {
		for target in pv.file_descriptors {
			if target.loop_id == pv.loop.id {
				// vfmt off
				read_events := (
					(if C.FD_ISSET(target.fd, &readfds) != 0 { picoev_read } else { 0 })
						|
					(if C.FD_ISSET(target.fd, &writefds) != 0 { picoev_write } else { 0 })
				)
				// vfmt on
				if read_events != 0 {
					$if trace_fd ? {
						eprintln('do callback ${target.fd}')
					}

					// do callback!
					unsafe { target.cb(target.fd, read_events, &pv) }
				}
			}
		}
	}

	return 0
}
