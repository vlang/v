module epoll

#include <sys/epoll.h>
#include <errno.h>

fn C.epoll_create1(__flags int) int
fn C.epoll_ctl(__epfd int, __op int, __fd int, __event &C.epoll_event) int
fn C.epoll_wait(__epfd int, __events &C.epoll_event, __maxevents int, __timeout int) int
fn C.perror(s &u8)
fn C.close(fd int) int

union C.epoll_data {
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

pub struct C.epoll_event {
	events u32
	data   C.epoll_data
}

// Callbacks for epoll-driven IO events.
pub struct EpollEventCallbacks {
pub:
	on_read  fn (fd int) @[required]
	on_write fn (fd int) @[required]
}

// Create a new epoll instance. Returns fd or <0 on error.
pub fn create() int {
	epoll_fd := C.epoll_create1(0)
	if epoll_fd < 0 {
		C.perror(c'epoll_create1')
	}
	return epoll_fd
}

// Add a file descriptor to an epoll instance with given event mask.
pub fn add_fd_to_epoll(epoll_fd int, fd int, events u32) int {
	mut ev := C.epoll_event{
		events: events
	}
	ev.data.fd = fd
	if C.epoll_ctl(epoll_fd, C.EPOLL_CTL_ADD, fd, &ev) == -1 {
		eprintln(@LOCATION)
		C.perror(c'epoll_ctl')
		return -1
	}
	return 0
}

// Remove a file descriptor from an epoll instance.
pub fn remove_fd_from_epoll(epoll_fd int, fd int) {
	C.epoll_ctl(epoll_fd, C.EPOLL_CTL_DEL, fd, C.NULL)
	C.close(fd)
}

pub fn close(epoll_fd int) {
	C.close(epoll_fd)
}
