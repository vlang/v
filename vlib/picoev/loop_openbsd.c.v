module picoev

#include <errno.h>
#include <sys/types.h>
#include <sys/event.h>

fn C.kevent(int, changelist voidptr, nchanges int, eventlist voidptr, nevents int, timeout &C.timespec) int
fn C.kqueue() int
fn C.EV_SET(kev voidptr, ident int, filter i16, flags u16, fflags u32, data voidptr, udata voidptr)

pub struct C.kevent {
pub mut:
	ident int
	// uintptr_t
	filter i16
	flags  u16
	fflags u32
	data   voidptr
	// intptr_t
	udata voidptr
}

@[heap]
pub struct KqueueLoop {
mut:
	id    int
	now   i64
	kq_id int
	// -1 if not changed
	changed_fds int
	events      [1024]C.kevent
	changelist  [256]C.kevent
}

type LoopType = KqueueLoop

// create_kqueue_loop creates a new kernel event queue with loop_id=`id`.
pub fn create_kqueue_loop(id int) !&KqueueLoop {
	mut loop := &KqueueLoop{
		id: id
	}

	loop.kq_id = C.kqueue()
	if loop.kq_id == -1 {
		return error('could not create kqueue loop!')
	}
	loop.changed_fds = -1
	return loop
}

// ev_set sets a new `kevent` with file descriptor `index`.
@[inline]
pub fn (mut pv Picoev) ev_set(index int, operation int, events int) {
	mut filter := 0
	if events & picoev_read != 0 {
		filter |= C.EVFILT_READ
	}
	if events & picoev_write != 0 {
		filter |= C.EVFILT_WRITE
	}
	filter = i16(filter)

	C.EV_SET(&pv.loop.changelist[index], pv.loop.changed_fds, filter, operation, 0, 0,
		0)
}

// backend_build uses the lower 8 bits to store the old events and the higher 8
// bits to store the next file descriptor in `Target.backend`.
@[inline]
fn backend_build(next_fd int, events u32) int {
	return int((u32(next_fd) << 8) | (events & 0xff))
}

// get the lower 8 bits
@[inline]
fn backend_get_old_events(backend int) int {
	return backend & 0xff
}

// get the higher 8 bits
@[inline]
fn backend_get_next_fd(backend int) int {
	return backend >> 8
}

// apply pending processes all changes for the file descriptors and updates `loop.changelist`
// if `aplly_all` is `true` the changes are immediately applied.
fn (mut pv Picoev) apply_pending_changes(apply_all bool) int {
	mut total, mut nevents := 0, 0

	for pv.loop.changed_fds != -1 {
		mut target := pv.file_descriptors[pv.loop.changed_fds]
		old_events := backend_get_old_events(target.backend)
		if target.events != old_events {
			// events have been changed
			if old_events != 0 {
				pv.ev_set(total, C.EV_DISABLE, old_events)
				total++
			}
			if target.events != 0 {
				pv.ev_set(total, C.EV_ADD | C.EV_ENABLE, int(target.events))
				total++
			}
			// Apply the changes if the total changes exceed the changelist size
			if total + 1 >= pv.loop.changelist.len {
				nevents = C.kevent(pv.loop.kq_id, &pv.loop.changelist, total, C.NULL,
					0, C.NULL)
				assert nevents == 0
				total = 0
			}
		}

		pv.loop.changed_fds = backend_get_next_fd(target.backend)
		target.backend = -1
	}

	if apply_all && total != 0 {
		nevents = C.kevent(pv.loop.kq_id, &pv.loop.changelist, total, C.NULL, 0, C.NULL)
		assert nevents == 0
		total = 0
	}

	return total
}

@[direct_array_access]
fn (mut pv Picoev) update_events(fd int, events int) int {
	// check if fd is in range
	assert fd < max_fds

	mut target := pv.file_descriptors[fd]

	// initialize if adding the fd
	if events & picoev_add != 0 {
		target.backend = -1
	}

	// return if nothing to do
	if (events == picoev_del && target.backend == -1)
		|| (events != picoev_del && events & picoev_readwrite == target.events) {
		return 0
	}

	// add to changed list if not yet being done
	if target.backend == -1 {
		target.backend = backend_build(pv.loop.changed_fds, target.events)
		pv.loop.changed_fds = fd
	}

	// update events
	target.events = u32(events & picoev_readwrite)
	// apply immediately if is a DELETE
	if events & picoev_del != 0 {
		pv.apply_pending_changes(true)
	}

	return 0
}

@[direct_array_access]
fn (mut pv Picoev) poll_once(max_wait_in_sec int) int {
	ts := C.timespec{
		tv_sec:  max_wait_in_sec
		tv_nsec: 0
	}

	mut total, mut nevents := 0, 0
	// apply changes later when the callback is called.
	total = pv.apply_pending_changes(false)

	nevents = C.kevent(pv.loop.kq_id, &pv.loop.changelist, total, &pv.loop.events, pv.loop.events.len,
		&ts)
	if nevents == -1 {
		// the errors we can only rescue
		assert C.errno == C.EACCES || C.errno == C.EFAULT || C.errno == C.EINTR
		return -1
	}

	for i := 0; i < nevents; i++ {
		event := pv.loop.events[i]
		target := pv.file_descriptors[event.ident]

		// changelist errors are fatal
		assert event.flags & C.EV_ERROR == 0

		if pv.loop.id == target.loop_id && event.filter & (C.EVFILT_READ | C.EVFILT_WRITE) != 0 {
			read_events := match int(event.filter) {
				C.EVFILT_READ {
					picoev_read
				}
				C.EVFILT_WRITE {
					picoev_write
				}
				else {
					0
				}
			}

			// do callback!
			unsafe { target.cb(target.fd, read_events, &pv) }
		}
	}

	return 0
}
