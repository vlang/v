module picoev

#include <sys/epoll.h>

$if !musl ? {
	#include <sys/cdefs.h> // needed for cross compiling to linux
}

fn C.epoll_create(__flags int) int
fn C.epoll_wait(__epfd int, __events &C.epoll_event, __maxevents int, __timeout int) int
fn C.epoll_ctl(__epfd int, __op int, __fd int, __event &C.epoll_event) int

@[typedef]
union C.epoll_data {
mut:
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

@[packed]
pub struct C.epoll_event {
	events u32
	data   C.epoll_data
}

@[heap]
pub struct EpollLoop {
mut:
	id       int
	epoll_fd int
	events   [1024]C.epoll_event
	now      i64
}

type LoopType = EpollLoop

// create_epoll_loop creates a new epoll instance and returns an `EpollLoop` struct with `id`.
pub fn create_epoll_loop(id int) !&EpollLoop {
	mut loop := &EpollLoop{
		id: id
	}

	loop.epoll_fd = C.epoll_create(max_fds)
	if loop.epoll_fd == -1 {
		return error('could not create epoll loop!')
	}

	return loop
}

// updates the events associated with a file descriptor in the event loop.
@[direct_array_access]
fn (mut pv Picoev) update_events(fd int, events int) int {
	// check if fd is in range
	assert fd < max_fds

	mut target := pv.file_descriptors[fd]
	mut ev := C.epoll_event{}

	// fd belongs to loop
	if events & picoev_del != target.events && target.loop_id != pv.loop.id {
		return -1
	}

	if events & picoev_readwrite == target.events {
		return 0
	}

	// vfmt off
	ev.events = u32(
		(if events & picoev_read != 0 { C.EPOLLIN } else { 0 })
			|
		(if events & picoev_write != 0 { C.EPOLLOUT } else { 0 })
	)
	// vfmt on
	ev.data.fd = fd

	if events & picoev_del != 0 {
		// nothing to do
	} else if events & picoev_readwrite == 0 {
		// delete the file if it exists
		epoll_ret := C.epoll_ctl(pv.loop.epoll_fd, C.EPOLL_CTL_DEL, fd, &ev)

		// check error
		assert epoll_ret == 0
	} else {
		// change settings to 0
		mut epoll_ret := C.epoll_ctl(pv.loop.epoll_fd, C.EPOLL_CTL_MOD, fd, &ev)
		if epoll_ret != 0 {
			// if the file is not present we want to add it
			assert C.errno == C.ENOENT
			epoll_ret = C.epoll_ctl(pv.loop.epoll_fd, C.EPOLL_CTL_ADD, fd, &ev)

			// check error
			assert epoll_ret == 0
		}
	}

	// convert to u32?
	target.events = u32(events)
	return 0
}

// performs a single iteration of the select-based event loop.
@[direct_array_access]
fn (mut pv Picoev) poll_once(max_wait_in_sec int) int {
	nevents := C.epoll_wait(pv.loop.epoll_fd, &pv.loop.events[0], max_fds, max_wait_in_sec * 1000)

	if nevents == -1 {
		// timeout has occurred
		return -1
	}

	for i := 0; i < nevents; i++ {
		mut event := pv.loop.events[i]
		target := unsafe { pv.file_descriptors[event.data.fd] }
		unsafe {
			assert event.data.fd < max_fds
		}
		if pv.loop.id == target.loop_id && target.events & picoev_readwrite != 0 {
			mut read_events := 0
			if event.events & u32(C.EPOLLIN) != 0 {
				read_events |= picoev_read
			}
			if event.events & u32(C.EPOLLOUT) != 0 {
				read_events |= picoev_write
			}

			if read_events != 0 {
				// do callback!
				unsafe { target.cb(event.data.fd, read_events, &pv) }
			}
		} else {
			// defer epoll delete
			event.events = 0
			unsafe {
				C.epoll_ctl(pv.loop.epoll_fd, C.EPOLL_CTL_DEL, event.data.fd, &event)
			}
		}
	}
	return 0
}
