module notify

import time
import os

#include <sys/epoll.h>

struct C.epoll_event {
	events u32
	data   C.epoll_data_t
}

[typedef]
union C.epoll_data_t {
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

fn C.epoll_create1(int) int

fn C.epoll_ctl(int, int, int, &C.epoll_event) int

fn C.epoll_wait(int, &C.epoll_event, int, int) int

[noinit]
struct EpollNotifier {
	epoll_fd int
mut:
	num_watching int // heuristic
	events       []C.epoll_event
}

[noinit]
struct EpollEvent {
pub:
	fd   int
	kind FdEventType
}

pub fn new() ?FdNotifier {
	fd := C.epoll_create1(0) // 0 indicates default behavior
	if fd == -1 {
		return error(os.posix_get_error_msg(C.errno))
	}
	// Needed to circumvent V limitations
	x := &EpollNotifier{
		epoll_fd: fd
	}
	return x
}

const (
	epoll_read         = u32(C.EPOLLIN)
	epoll_write        = u32(C.EPOLLOUT)
	epoll_peer_hangup  = u32(C.EPOLLRDHUP)
	epoll_exception    = u32(C.EPOLLPRI)
	epoll_error        = u32(C.EPOLLERR)
	epoll_hangup       = u32(C.EPOLLHUP)
	epoll_edge_trigger = u32(C.EPOLLET)
	epoll_one_shot     = u32(C.EPOLLONESHOT)
	epoll_wake_up      = u32(C.EPOLLWAKEUP)
	epoll_exclusive    = u32(C.EPOLLEXCLUSIVE)
)

fn (mut en EpollNotifier) ctl(fd int, op int, mask u32) ? {
	event := C.epoll_event{
		events: mask
		data: C.epoll_data_t{
			fd: fd
		}
	}
	if C.epoll_ctl(en.epoll_fd, op, fd, &event) == -1 {
		return error(os.posix_get_error_msg(C.errno))
	}
}

fn (mut en EpollNotifier) add(fd int, events FdEventType, conf ...FdConfigFlags) ? {
	mask := flags_to_mask(events, ...conf)
	en.ctl(fd, C.EPOLL_CTL_ADD, mask) ?
	en.num_watching++
}

fn (mut en EpollNotifier) modify(fd int, events FdEventType, conf ...FdConfigFlags) ? {
	mask := flags_to_mask(events, ...conf)
	en.ctl(fd, C.EPOLL_CTL_MOD, mask) ?
}

fn (mut en EpollNotifier) remove(fd int) ? {
	en.ctl(fd, C.EPOLL_CTL_DEL, 0) ?
	if en.num_watching > 0 {
		en.num_watching--
	}
}

[direct_array_access]
fn (mut en EpollNotifier) wait(timeout time.Duration) []FdEvent {
	if en.events.cap < en.num_watching {
		en.events.grow_cap(en.num_watching - en.events.cap)
	}
	// populate en.events.data with the new events
	to := match timeout {
		time.infinite { -1 }
		else { int(timeout / time.millisecond) }
	}
	count := C.epoll_wait(en.epoll_fd, en.events.data, en.events.cap, to)

	// set len to count
	en.events.clear()
	unsafe { en.events.grow_len(count) }

	if count > 0 {
		mut arr := []FdEvent{cap: count}
		for i := 0; i < count; i++ {
			fd := unsafe { en.events[i].data.fd }
			kind := event_mask_to_flag(en.events[i].events)
			if kind.is_empty() {
				// NOTE: tcc only reports the first event for some
				// reason, leaving subsequent structs in the array as 0
				// (or possibly garbage)
				panic('encountered an empty event kind; this is most likely due to using tcc')
			}
			arr << &EpollEvent{
				fd: fd
				kind: kind
			}
		}
		return arr
	}
	return []
}

fn (mut en EpollNotifier) close() ? {
	if C.close(en.epoll_fd) == -1 {
		return error(os.posix_get_error_msg(C.errno))
	}
}

fn event_mask_to_flag(mask u32) FdEventType {
	mut flags := FdEventType{}

	if mask & notify.epoll_read != 0 {
		flags.set(.read)
	}
	if mask & notify.epoll_write != 0 {
		flags.set(.write)
	}
	if mask & notify.epoll_peer_hangup != 0 {
		flags.set(.peer_hangup)
	}
	if mask & notify.epoll_exception != 0 {
		flags.set(.exception)
	}
	if mask & notify.epoll_error != 0 {
		flags.set(.error)
	}
	if mask & notify.epoll_hangup != 0 {
		flags.set(.hangup)
	}

	return flags
}

fn flags_to_mask(events FdEventType, confs ...FdConfigFlags) u32 {
	mut mask := u32(0)
	if events.has(.read) {
		mask |= notify.epoll_read
	}
	if events.has(.write) {
		mask |= notify.epoll_write
	}
	if events.has(.peer_hangup) {
		mask |= notify.epoll_peer_hangup
	}
	if events.has(.exception) {
		mask |= notify.epoll_exception
	}
	if events.has(.error) {
		mask |= notify.epoll_error
	}
	if events.has(.hangup) {
		mask |= notify.epoll_hangup
	}
	for conf in confs {
		if conf.has(.edge_trigger) {
			mask |= notify.epoll_edge_trigger
		}
		if conf.has(.one_shot) {
			mask |= notify.epoll_one_shot
		}
		if conf.has(.wake_up) {
			mask |= notify.epoll_wake_up
		}
		if conf.has(.exclusive) {
			mask |= notify.epoll_exclusive
		}
	}
	return mask
}
