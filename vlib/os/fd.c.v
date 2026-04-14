module os

// low level operations with file descriptors/handles

$if !windows {
	#include <sys/select.h>
	#include <sys/ioctl.h>
}

$if windows {
	#include <winsock2.h>
}

#flag windows -lws2_32

// fd_close closes the file descriptor. It returns 0 on success.
pub fn fd_close(fd int) int {
	if fd == -1 {
		return 0
	}
	return C.close(fd)
}

// fd_write writes the given string to the file descriptor.
// It blocks until all the data in the string is written.
pub fn fd_write(fd int, s string) {
	if fd == -1 {
		return
	}
	mut sp := s.str
	mut remaining := s.len
	for remaining > 0 {
		written := int(C.write(fd, sp, remaining))
		if written < 0 {
			return
		}
		remaining = remaining - written
		sp = unsafe { voidptr(usize(sp) + usize(written)) }
	}
}

// fd_slurp reads all the remaining data from the file descriptor.
pub fn fd_slurp(fd int) []string {
	mut res := []string{}
	if fd == -1 {
		return res
	}
	for {
		s, b := fd_read(fd, 4096)
		if b <= 0 {
			break
		}
		res << s
	}
	return res
}

// fd_read reads data from the file descriptor. It returns the read data, and how many bytes were read.
pub fn fd_read(fd int, maxbytes int) (string, int) {
	if fd == -1 {
		return '', 0
	}
	unsafe {
		mut buf := malloc_noscan(maxbytes + 1)
		nbytes := int(C.read(fd, buf, maxbytes))
		if nbytes < 0 {
			free(buf)
			return '', nbytes
		}
		buf[nbytes] = 0
		return tos(buf, nbytes), nbytes
	}
}

@[typedef]
pub struct C.fd_set {}

pub struct C.timeval {
pub:
	tv_sec  u64
	tv_usec u64
}

fn C.select(ndfs i32, readfds &C.fd_set, writefds &C.fd_set, exceptfds &C.fd_set, timeout &C.timeval) i32

$if !windows {
	fn C.ioctl(fd i32, request u64, args ...voidptr) i32
}

// These are C macros, but from the V's point of view, can be treated as C functions:
fn C.FD_ZERO(fdset &C.fd_set)
fn C.FD_SET(fd i32, fdset &C.fd_set)
fn C.FD_ISSET(fd i32, fdset &C.fd_set) i32

// fd_is_pending returns true, when there is pending data, waiting to be read from file descriptor `fd`.
// If the file descriptor is closed, or if reading from it, will block (there is no data), fd_is_pending returns false.
pub fn fd_is_pending(fd int) bool {
	if fd == -1 {
		return false
	}
	mut bytes_avail := int(0)
	$if windows {
		handle := voidptr(C._get_osfhandle(fd))
		if handle != voidptr(-1) {
			if C.PeekNamedPipe(handle, unsafe { nil }, int(0), unsafe { nil },
				voidptr(&bytes_avail), unsafe { nil })
			{
				return bytes_avail > 0
			}
		}
	} $else {
		if C.ioctl(fd, u64(C.FIONREAD), &bytes_avail) == 0 {
			return bytes_avail > 0
		}
	}
	read_set := C.fd_set{}
	C.FD_ZERO(&read_set)
	C.FD_SET(fd, &read_set)
	mut ts := C.timeval{
		tv_sec:  0
		tv_usec: 0
	}
	res := C.select(fd + 1, &read_set, C.NULL, C.NULL, &ts)
	if res > 0 {
		if C.FD_ISSET(fd, &read_set) != 0 {
			return true
		}
	}
	return false
}
