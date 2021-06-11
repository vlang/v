module events

import time

#include <sys/epoll.h>

struct C.epoll_event {
	events u32
	data   C.epoll_data_t
}

[typedef]
union C.epoll_data_t {
	ptr voidptr
	fd int
	u32 u32
	u64 u64
}

fn C.epoll_create1(int) int

fn C.epoll_ctl(int, int, int, &C.epoll_event) int

fn C.epoll_wait(int, &C.epoll_event, int, int) int

[flag]
pub enum EpollEventType {
	epoll_read
	epoll_write
	epoll_peer_hangup
	epoll_exceptional
	epoll_error
	epoll_hangup
	epoll_edge_trigger
	epoll_one_shot
	epoll_wake_up
	epoll_exclusive
}

[noinit]
pub struct EpollNotifier {
	epoll_fd int
mut:
	num_watching int // TODO: change to a set to make add/remove safer
	events       []C.epoll_event
}

[noinit]
pub struct EpollEvent {
pub:
	fd   int
	kind EpollEventType
}

pub fn new_notifier() ?EpollNotifier {
	fd := C.epoll_create1(0) // 0 indicates default behavior
	if fd == -1 {
		return error(posix_get_error_msg(C.errno))
	}
	return EpollNotifier{
		epoll_fd: fd
		num_watching: 0
	}
}

pub const (
	epoll_read         = u32(C.EPOLLIN)
	epoll_write        = u32(C.EPOLLOUT)
	epoll_peer_hangup  = u32(C.EPOLLRDHUP)
	epoll_exceptional  = u32(C.EPOLLPRI)
	epoll_error        = u32(C.EPOLLERR)
	epoll_hangup       = u32(C.EPOLLHUP)
	epoll_edge_trigger = u32(C.EPOLLET)
	epoll_one_shot     = u32(C.EPOLLONESHOT)
	epoll_wake_up      = u32(C.EPOLLWAKEUP)
	epoll_exclusive    = u32(C.EPOLLEXCLUSIVE)
)

pub fn (mut en EpollNotifier) add(fd int, flags u32) ? {
	event := C.epoll_event{
		events: flags
		data: C.epoll_data_t{ fd: fd }
	}
	if C.epoll_ctl(en.epoll_fd, C.EPOLL_CTL_ADD, fd, &event) == -1 {
		return error(posix_get_error_msg(C.errno))
	}
	en.num_watching++
}

pub fn (mut en EpollNotifier) remove(fd int) ? {
	if C.epoll_ctl(en.epoll_fd, C.EPOLL_CTL_DEL, fd, 0) == -1 {
		return error(posix_get_error_msg(C.errno))
	}
	en.num_watching--
}

[direct_array_access]
pub fn (mut en EpollNotifier) wait(timeout time.Duration) []EpollEvent {
	if en.events.cap < en.num_watching {
		en.events.grow_cap(en.num_watching - en.events.cap)
	}
	// populate en.events.data with the new events
	count := C.epoll_wait(en.epoll_fd, en.events.data, en.events.cap, int(timeout / time.millisecond))

	// set len to count
	en.events.clear()
	unsafe { en.events.grow_len(count) }

	if count > 0 {
		mut arr := []EpollEvent{cap: count}
		for i := 0; i < count; i++ {
			fd := unsafe { en.events[i].data.fd }
			kind := event_mask_to_flag(en.events[i].events)
			if kind.is_empty() {
				// NOTE: tcc only reports the first event for some
				// reason, leaving subsequent structs in the array as 0
				// (or possibly garbage)
				panic('encountered an empty event kind; this is most likely due to using tcc')
			}
			arr << EpollEvent{
				fd: fd
				kind: kind
			}
		}
		return arr
	}
	return []
}

fn event_mask_to_flag(mask u32) EpollEventType {
	mut flags := EpollEventType{}

	if mask & events.epoll_read != 0 {
		flags.set(.epoll_read)
	}
	if mask & events.epoll_write != 0 {
		flags.set(.epoll_write)
	}
	if mask & events.epoll_peer_hangup != 0 {
		flags.set(.epoll_peer_hangup)
	}
	if mask & events.epoll_exceptional != 0 {
		flags.set(.epoll_exceptional)
	}
	if mask & events.epoll_error != 0 {
		flags.set(.epoll_error)
	}
	if mask & events.epoll_hangup != 0 {
		flags.set(.epoll_hangup)
	}
	// the rest of the flags are for configuration and will never
	// be returned by epoll_wait

	return flags
}

pub fn (mut en EpollNotifier) close() ? {
	if C.close(en.epoll_fd) == -1 {
		return error(posix_get_error_msg(C.errno))
	}
}
