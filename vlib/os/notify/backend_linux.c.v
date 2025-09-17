module notify

import time
import os

#insert "@VEXEROOT/vlib/os/notify/epoll.h"

pub struct C.epoll_event {
	events u32
	data   C.epoll_data_t
}

@[typedef]
union C.epoll_data_t {
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

fn C.epoll_create1(int) int

fn C.epoll_ctl(int, int, int, &C.epoll_event) int

fn C.epoll_wait(int, &C.epoll_event, int, int) int

// EpollNotifier provides methods that implement FdNotifier using the
// epoll I/O event notification facility (linux only)
struct EpollNotifier {
	epoll_fd int
}

// EpollEvent describes an event that occurred for a file descriptor in
// the watch list
struct EpollEvent {
pub:
	fd   int
	kind FdEventType
}

// new creates a new EpollNotifier.
// The FdNotifier interface is returned to allow OS specific
// implementations without exposing the concrete type
pub fn new() !FdNotifier {
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

const epoll_read = u32(C.EPOLLIN)
const epoll_write = u32(C.EPOLLOUT)
const epoll_peer_hangup = u32(C.EPOLLRDHUP)
const epoll_exception = u32(C.EPOLLPRI)
const epoll_error = u32(C.EPOLLERR)
const epoll_hangup = u32(C.EPOLLHUP)
const epoll_edge_trigger = u32(C.EPOLLET)
const epoll_one_shot = u32(C.EPOLLONESHOT)
const epoll_wake_up = u32(C.EPOLLWAKEUP)
const epoll_exclusive = u32(C.EPOLLEXCLUSIVE)

// ctl is a helper method for add, modify, and remove
fn (mut en EpollNotifier) ctl(fd int, op int, mask u32) ! {
	event := C.epoll_event{
		events: mask
		data:   C.epoll_data_t{
			fd: fd
		}
	}
	if C.epoll_ctl(en.epoll_fd, op, fd, &event) == -1 {
		return error(os.posix_get_error_msg(C.errno))
	}
}

// add adds a file descriptor to the watch list
fn (mut en EpollNotifier) add(fd int, events FdEventType, conf ...FdConfigFlags) ! {
	mask := flags_to_mask(events, ...conf)
	en.ctl(fd, C.EPOLL_CTL_ADD, mask)!
}

// modify sets an existing entry in the watch list to the provided events and configuration
fn (mut en EpollNotifier) modify(fd int, events FdEventType, conf ...FdConfigFlags) ! {
	mask := flags_to_mask(events, ...conf)
	en.ctl(fd, C.EPOLL_CTL_MOD, mask)!
}

// remove removes a file descriptor from the watch list
fn (mut en EpollNotifier) remove(fd int) ! {
	en.ctl(fd, C.EPOLL_CTL_DEL, 0)!
}

// wait waits to be notified of events on the watch list,
// returns at most 512 events
fn (mut en EpollNotifier) wait(timeout time.Duration) []FdEvent {
	// arbitrary 512 limit; events will round robin on successive
	// waits if the number exceeds this
	// NOTE: we use a fixed size array here for stack allocation; this has
	//       the added bonus of making EpollNotifier thread safe
	events := [512]C.epoll_event{}
	// populate events with the new events
	to := timeout.sys_milliseconds()
	count := C.epoll_wait(en.epoll_fd, &events[0], events.len, to)

	if count > 0 {
		mut arr := []FdEvent{cap: count}
		for i := 0; i < count; i++ {
			fd := unsafe { events[i].data.fd }
			kind := event_mask_to_flag(events[i].events)
			if kind.is_empty() {
				// NOTE: tcc only reports the first event for some
				// reason, leaving subsequent structs in the array as 0
				// (or possibly garbage)
				panic('encountered an empty event kind; this is most likely due to using tcc')
			}
			arr << &EpollEvent{
				fd:   fd
				kind: kind
			}
		}
		return arr
	}
	return []
}

// close closes the EpollNotifier,
// any successive calls to add, modify, remove, and wait should fail
fn (mut en EpollNotifier) close() ! {
	if C.close(en.epoll_fd) == -1 {
		return error(os.posix_get_error_msg(C.errno))
	}
}

// event_mask_to_flag is a helper function that converts a bitmask
// returned by epoll_wait to FdEventType
fn event_mask_to_flag(mask u32) FdEventType {
	mut flags := unsafe { FdEventType(0) }

	if mask & epoll_read != 0 {
		flags.set(.read)
	}
	if mask & epoll_write != 0 {
		flags.set(.write)
	}
	if mask & epoll_peer_hangup != 0 {
		flags.set(.peer_hangup)
	}
	if mask & epoll_exception != 0 {
		flags.set(.exception)
	}
	if mask & epoll_error != 0 {
		flags.set(.error)
	}
	if mask & epoll_hangup != 0 {
		flags.set(.hangup)
	}

	return flags
}

// flags_to_mask is a helper function that converts FdEventType and
// FdConfigFlags to a bitmask used by the C functions
fn flags_to_mask(events FdEventType, confs ...FdConfigFlags) u32 {
	mut mask := u32(0)
	if events.has(.read) {
		mask |= epoll_read
	}
	if events.has(.write) {
		mask |= epoll_write
	}
	if events.has(.peer_hangup) {
		mask |= epoll_peer_hangup
	}
	if events.has(.exception) {
		mask |= epoll_exception
	}
	if events.has(.error) {
		mask |= epoll_error
	}
	if events.has(.hangup) {
		mask |= epoll_hangup
	}
	for conf in confs {
		if conf.has(.edge_trigger) {
			mask |= epoll_edge_trigger
		}
		if conf.has(.one_shot) {
			mask |= epoll_one_shot
		}
		if conf.has(.wake_up) {
			mask |= epoll_wake_up
		}
		if conf.has(.exclusive) {
			mask |= epoll_exclusive
		}
	}
	return mask
}
