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

// C.epoll_event represents an event structure used by epoll.
//
// The 'events' field is a bitmask of event flags that epoll will monitor for the file descriptor.
// Common event flags include:
//   EPOLLIN      (0x001):  The associated file is available for read operations.
//   EPOLLOUT     (0x004):  The associated file is available for write operations.
//   EPOLLRDHUP   (0x2000): Stream peer closed connection, or shut down writing half.
//   EPOLLPRI     (0x002):  There is urgent data to read (e.g., out-of-band data on TCP socket).
//   EPOLLERR     (0x008):  Error condition happened on the associated file descriptor.
//   EPOLLHUP     (0x010):  Hang up happened on the associated file descriptor.
//   EPOLLET      (1 << 31): Set Edge Triggered behavior, instead of Level Triggered.
//   EPOLLONESHOT (1 << 30): Set one-shot behavior for the monitored fd.
//
// You can combine these flags using bitwise OR (|) to monitor multiple events.
struct C.epoll_event {
	events u32 // Bitmask of epoll event flags (see above)
	data   C.epoll_data
}

// EpollFd represents an epoll instance file descriptor.
pub type EpollFd = int

// EpollEvent represents an epoll event structure.
pub type EpollEvent = C.epoll_event

// Create a new epoll instance. Returns fd or <0 on error.
pub fn create() EpollFd {
	epoll_fd := C.epoll_create1(0)
	if epoll_fd < 0 {
		C.perror(c'epoll_create1')
	}
	return EpollFd(epoll_fd)
}

// Add a file descriptor to an epoll instance with the given event mask.
//
// 'events' is a bitmask of epoll event flags to monitor for the file descriptor.
// For example, to monitor for readable events, use EPOLLIN (1).
// You can combine multiple flags using bitwise OR (e.g., EPOLLIN | EPOLLOUT).
// See the documentation for C.epoll_event for possible values.
pub fn (epoll_fd EpollFd) add_fd(fd int, events u32) int {
	mut ev := C.epoll_event{
		events: events
	}
	ev.data.fd = fd
	if C.epoll_ctl(int(epoll_fd), C.EPOLL_CTL_ADD, fd, &ev) == -1 {
		eprintln(@LOCATION)
		C.perror(c'epoll_ctl')
		return -1
	}
	return 0
}

// Remove a file descriptor from an epoll instance.
pub fn (epoll_fd EpollFd) remove_fd(fd int) {
	C.epoll_ctl(int(epoll_fd), C.EPOLL_CTL_DEL, fd, C.NULL)
	C.close(fd)
}

// Wait for events on the epoll instance.
pub fn (epoll_fd EpollFd) wait_for_events(mut events EpollEvent, maxevents int, timeout int) int {
	n := C.epoll_wait(int(epoll_fd), &events, maxevents, timeout)
	if n == -1 {
		C.perror(c'epoll_wait')
	}
	return n
}

// Close the epoll instance.
pub fn (epoll_fd EpollFd) close() {
	C.close(int(epoll_fd))
}
