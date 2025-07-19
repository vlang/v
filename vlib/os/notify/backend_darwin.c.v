module notify

import time
import os

#insert "@VEXEROOT/vlib/os/notify/kqueue.h"

pub struct C.kevent {
mut:
	ident  u32
	filter i16
	flags  u16
	fflags u32
	data   int
	udata  voidptr
}

fn C.kqueue() int
fn C.__kevent__(int, voidptr, int, voidptr, int, voidptr) int
fn C.EV_SET(voidptr, u32, i16, u16, u32, int, voidptr)

// KqueueNotifier provides methods that implement FdNotifier using the
// kqueue I/O event notification facility (macos, freeBSD, xxxBSD...unix only)
struct KqueueNotifier {
	kqueue_fd int
}

// KqueueEvent describes an event that occurred for a file descriptor in
// the watch list
struct KqueueEvent {
pub:
	fd   int
	kind FdEventType
}

// new creates a new KqueueNotifier.
// The FdNotifier interface is returned to allow OS specific
// implementations without exposing the concrete type
pub fn new() !FdNotifier {
	fd := C.kqueue()
	if fd == -1 {
		return error(os.posix_get_error_msg(C.errno))
	}
	// Needed to circumvent V limitations
	x := &KqueueNotifier{
		kqueue_fd: fd
	}
	return x
}

// filter types
const kqueue_read = i16(C.EVFILT_READ)
const kqueue_write = i16(C.EVFILT_WRITE)
const kqueue_aio = i16(C.EVFILT_AIO)
const kqueue_vnode = i16(C.EVFILT_VNODE)
const kqueue_proc = i16(C.EVFILT_PROC)
const kqueue_signal = i16(C.EVFILT_SIGNAL)
const kqueue_timer = i16(C.EVFILT_TIMER)
const kqueue_machport = i16(C.EVFILT_MACHPORT)
const kqueue_fs = i16(C.EVFILT_FS)
const kqueue_user = i16(C.EVFILT_USER)
const kqueue_vm = i16(C.EVFILT_VM)
const kqueue_exception = i16(C.EVFILT_EXCEPT)
const kqueue_syscount = i16(C.EVFILT_SYSCOUNT)

// actions
const kqueue_add = u16(C.EV_ADD)
const kqueue_delete = u16(C.EV_DELETE)
const kqueue_enable = u16(C.EV_ENABLE)
const kqueue_disable = u16(C.EV_DISABLE)

// flags
const kqueue_oneshot = u16(C.EV_ONESHOT)
const kqueue_edge_trigger = u16(C.EV_CLEAR) // kqueue_clear

const kqueue_receipt = u16(C.EV_RECEIPT)
const kqueue_dispatch = u16(C.EV_DISPATCH)
const kqueue_udata_specific = u16(C.EV_UDATA_SPECIFIC)
const kqueue_dispatch2 = u16(C.EV_DISPATCH | C.EV_UDATA_SPECIFIC)
const kqueue_vanished = u16(C.EV_VANISHED)
const kqueue_sysflags = u16(C.EV_SYSFLAGS)
const kqueue_flag0 = u16(C.EV_FLAG0)
const kqueue_flag1 = u16(C.EV_FLAG1)

// returned values
const kqueue_eof = u16(C.EV_EOF)
const kqueue_error = u16(C.EV_ERROR)

// ctl is a helper method for add, modify, and remove
fn (mut kn KqueueNotifier) ctl(fd int, filter i16, flags u16) ! {
	event := [1]C.kevent{}
	C.EV_SET(&event[0], fd, filter, flags, 0, 0, unsafe { nil })
	if C.__kevent__(kn.kqueue_fd, &event[0], 1, unsafe { nil }, 0, unsafe { nil }) == -1 {
		return error(os.posix_get_error_msg(C.errno))
	}
}

// add adds a file descriptor to the watch list
fn (mut kn KqueueNotifier) add(fd int, events FdEventType, conf ...FdConfigFlags) ! {
	filter := filter_to_mask(events)
	flags := flags_to_mask(...conf)
	kn.ctl(fd, filter, flags)!
}

// modify sets an existing entry in the watch list to the provided events and configuration
fn (mut kn KqueueNotifier) modify(fd int, events FdEventType, conf ...FdConfigFlags) ! {
	kn.add(fd, events, ...conf)!
}

// remove removes a file descriptor from the watch list
fn (mut kn KqueueNotifier) remove(fd int) ! {
	filter := kqueue_read | kqueue_write | kqueue_exception
	flags := kqueue_delete
	kn.ctl(fd, filter, flags)!
}

// wait waits to be notified of events on the watch list,
// returns at most 512 events
fn (mut kn KqueueNotifier) wait(timeout time.Duration) []FdEvent {
	// arbitrary 512 limit; events will round robin on successive
	// waits if the number exceeds this
	// NOTE: we use a fixed size array here for stack allocation; this has
	//       the added bonus of making KqueueNotifier thread safe
	events := [512]C.kevent{}
	// populate events with the new events
	to := &C.timespec{0, timeout.nanoseconds()}
	count := C.__kevent__(kn.kqueue_fd, unsafe { nil }, 0, &events[0], events.len, to)

	if count > 0 {
		mut arr := []FdEvent{cap: count}
		for i := 0; i < count; i++ {
			fd := int(events[i].ident)
			kind := event_mask_to_flag(events[i].filter, events[i].flags)
			if kind.is_empty() {
				// NOTE: tcc only reports the first event for some
				// reason, leaving subsequent structs in the array as 0
				// (or possibly garbage)
				panic('encountered an empty event kind; this is most likely due to using tcc')
			}
			arr << &KqueueEvent{
				fd:   fd
				kind: kind
			}
		}
		return arr
	}
	return []
}

// close closes the KqueueNotifier,
// any successive calls to add, modify, remove, and wait should fail
fn (mut kn KqueueNotifier) close() ! {
	if C.close(kn.kqueue_fd) == -1 {
		return error(os.posix_get_error_msg(C.errno))
	}
}

// event_mask_to_flag is a helper function that converts a bitmask
// returned by kevent() wait to FdEventType
fn event_mask_to_flag(filter i16, flags u16) FdEventType {
	mut res := unsafe { FdEventType(0) }

	if filter & kqueue_read != 0 {
		res.set(.read)
	}
	if filter & kqueue_write != 0 {
		res.set(.write)
	}
	if filter & kqueue_exception != 0 {
		res.set(.exception)
	}

	if flags & kqueue_eof != 0 {
		res.set(.hangup)
	}
	if flags & kqueue_error != 0 {
		res.set(.error)
	}

	return res
}

// filter_to_mask is a helper function that converts FdEventType
// to a bitmask used by the C functions
fn filter_to_mask(events FdEventType) i16 {
	mut mask := i16(0)
	if events.has(.read) {
		mask |= kqueue_read
	}
	if events.has(.write) {
		mask |= kqueue_write
	}
	if events.has(.exception) {
		mask |= kqueue_exception
	}
	if events.has(.peer_hangup) {
		panic("Kqueue does not support 'peer_hangup' event type.")
	}
	if events.has(.error) {
		panic("Kqueue does not support 'error' event type.")
	}
	if events.has(.hangup) {
		panic("Kqueue does not support 'hangup' event type.")
	}
	return mask
}

// flags_to_mask is a helper function that converts FdConfigFlags
// to a bitmask used by the C functions
fn flags_to_mask(confs ...FdConfigFlags) u16 {
	mut mask := kqueue_add | kqueue_enable
	for conf in confs {
		if conf.has(.edge_trigger) {
			mask |= kqueue_edge_trigger
		}
		if conf.has(.one_shot) {
			mask |= kqueue_oneshot
		}
		if conf.has(.wake_up) {
			panic("Kqueue does not support 'wake_up' flag.")
		}
		if conf.has(.exclusive) {
			panic("Kqueue does not support 'exclusive' flag.")
		}
	}
	return mask
}
