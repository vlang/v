module net

// Hosted `-d vlibc` UDP bridge.
//
// When V is built with `-d vlibc` (hosted, C backend), the AF_INET datagram
// subset (SOCK_DGRAM socket create, bind, sendto, recvfrom on 127.0.0.1) goes
// through the vlibc syscalls, while the sockaddr bytes stay shared with the
// libc path (net.Addr layout). Only `.ip` sockets are bridged; `.ip6`/`.unix`
// keep the libc path at the call sites in udp.c.v. DNS resolution (fuzzy
// resolve), multicast, and all `$if windows` branches are untouched.
// setsockopt/getsockopt/getsockname/fcntl go through vlibc on Linux
// (see sockopt_d_vlibc.c.v); getpeername stays on libc everywhere (no
// vlibc_sys_getpeername wrapper exists yet).

// vlibc_new_udp_socket creates a SOCK_DGRAM socket via the vlibc socket syscall.
fn vlibc_new_udp_socket(family AddrFamily) !int {
	fd, e := vlibc_socket(int(family), int(SocketType.udp), 0)
	vlibc_wrap_err(e)!
	return int(fd)
}

// vlibc_udp_bind binds an AF_INET datagram socket via vlibc_bind.
fn vlibc_udp_bind(handle int, a Addr) ! {
	port, addr := vlibc_ipv4_split(a)!
	// vlibc_bind stores sa.addr as-is, so convert the network-order value
	// to the in-memory representation first.
	vlibc_wrap_err(vlibc_bind(i64(handle), port, vlibc_htonl(addr)))!
}

// vlibc_udp_sendto sends `len` bytes from `b` to `addr` via the sendto syscall.
// Unlike vlibc_tcp_send, a zero-length `len` still issues the syscall: an
// empty datagram is a datagram, not the end of a stream.
fn vlibc_udp_sendto(handle int, b &u8, len int, flags int, addr Addr) !int {
	n, e := vlibc_sys_sendto(i64(handle), voidptr(b), u64(len), flags, voidptr(&addr),
		addr.len())
	vlibc_wrap_err(e)!
	return int(n)
}

// vlibc_udp_recvfrom receives up to `len` bytes into `buf_ptr` via the
// recvfrom syscall, filling in `addr` with the source address.
fn vlibc_udp_recvfrom(handle int, buf_ptr &u8, len int, flags int, mut addr Addr) !int {
	mut alen := u32(sizeof(Addr))
	n, e := vlibc_sys_recvfrom(i64(handle), voidptr(buf_ptr), u64(len), flags, voidptr(&addr),
		&alen)
	vlibc_wrap_err(e)!
	return int(n)
}

// vlibc_udp_write_to_ptr sends via vlibc_udp_sendto, waiting once for
// writability on EWOULDBLOCK like the libc path in write_to_ptr.
fn vlibc_udp_write_to_ptr(mut c UdpConn, addr Addr, b &u8, len int) !int {
	mut res := vlibc_udp_sendto(c.sock.handle, b, len, 0, addr) or {
		code := err.code()
		if code == int(error_ewouldblock) {
			c.wait_for_write()!
			return vlibc_udp_sendto(c.sock.handle, b, len, 0, addr) or {
				wrap_error(err.code())!
				-1
			}
		}
		wrap_error(code)!
		return error('none')
	}
	return res
}

// vlibc_udp_read_ptr receives via vlibc_udp_recvfrom, retrying once after
// waiting for readability like the libc path in read_ptr. A datagram may be
// empty: 0 bytes is a datagram received, not the end of a stream.
fn vlibc_udp_read_ptr(c &UdpConn, buf_ptr &u8, len int, mut addr Addr) !(int, Addr) {
	mut res := vlibc_udp_recvfrom(c.sock.handle, buf_ptr, len, 0, mut addr) or {
		code := err.code()
		if code in [int(error_ewouldblock), int(error_eagain), C.EINTR] {
			c.wait_for_read()!
			res2 := vlibc_udp_recvfrom(c.sock.handle, buf_ptr, len, 0, mut addr) or {
				wrap_error(err.code())!
				-1
			}
			return socket_error(res2)!, addr
		}
		wrap_error(code)!
		return error('none')
	}
	return res, addr
}
