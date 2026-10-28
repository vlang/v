module unix

import net

// Hosted `-d vlibc` AF_UNIX stream bridge.
//
// When V is built with `-d vlibc` (hosted, C backend), the unix stream subset
// (AF_UNIX socket create, bind, listen, accept, connect, send, recv) goes
// through the vlibc syscalls, while the sockaddr_un bytes stay shared with
// the libc path (net.Addr layout, passed by pointer with addr.len()).
// unlink/select/fcntl/close stay on libc: the fds created via raw syscalls
// live in the same process table and libc is still linked in hosted mode.
// setsockopt/getpeername/getsockname stay on libc as well: there is no
// portable one-line vlibc helper (vlibc_sys_setsockopt and
// vlibc_sys_getsockname exist on linux_bare only).

@[inline]
fn vlibc_wrap_err(e VlibcErrno) ! {
	if e != .enoerror {
		net.wrap_error(int(e))!
	}
}

// vlibc_unix_socket creates an AF_UNIX stream socket via the vlibc socket syscall.
fn vlibc_unix_socket() !int {
	fd, e := vlibc_socket(int(net.AddrFamily.unix), int(net.SocketType.tcp), 0)
	vlibc_wrap_err(e)!
	return int(fd)
}

// vlibc_unix_bind binds an AF_UNIX socket with the shared sockaddr_un bytes.
fn vlibc_unix_bind(handle int, a net.Addr) ! {
	vlibc_wrap_err(vlibc_sys_bind(i64(handle), voidptr(&a), a.len()))!
}

// vlibc_unix_connect connects an AF_UNIX socket with the shared sockaddr_un bytes.
fn vlibc_unix_connect(handle int, a net.Addr) ! {
	vlibc_wrap_err(vlibc_sys_connect(i64(handle), voidptr(&a), a.len()))!
}

// vlibc_unix_listen marks a bound AF_UNIX socket as listening.
fn vlibc_unix_listen(handle int, backlog int) ! {
	vlibc_wrap_err(vlibc_listen(i64(handle), backlog))!
}

// vlibc_unix_accept accepts one connection via vlibc_accept.
fn vlibc_unix_accept(handle int) !int {
	fd, e := vlibc_accept(i64(handle))
	vlibc_wrap_err(e)!
	if fd <= 0 {
		return error('accept failed')
	}
	return int(fd)
}

// vlibc_unix_send sends `len` bytes from `b` via vlibc_send.
fn vlibc_unix_send(handle int, b &u8, len int, flags int) !int {
	if len <= 0 {
		return 0
	}
	data := unsafe { b.vbytes(len) }
	n, e := vlibc_send(i64(handle), data, flags)
	vlibc_wrap_err(e)!
	return int(n)
}

// vlibc_unix_recv receives up to `len` bytes into `buf_ptr` via vlibc_recv.
fn vlibc_unix_recv(handle int, buf_ptr &u8, len int, flags int) !int {
	if len <= 0 {
		return 0
	}
	buf := unsafe { buf_ptr.vbytes(len) }
	n, e := vlibc_recv(i64(handle), buf, flags)
	vlibc_wrap_err(e)!
	return int(n)
}
