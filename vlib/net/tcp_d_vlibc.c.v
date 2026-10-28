module net

// Hosted `-d vlibc` TCP bridge.
//
// When V is built with `-d vlibc` (hosted, C backend), the pure-V vlibc
// syscall layer from builtin is linked in (see the `vlibc` block in
// vlib/v/driver/driver.v). The helpers below route the minimal coherent
// TCP subset (AF_INET socket create, bind, listen, accept, connect,
// send, recv, shutdown on 127.0.0.1) through those syscalls instead of
// libc: vlibc_socket, vlibc_bind, vlibc_listen, vlibc_accept,
// vlibc_connect, vlibc_send, vlibc_recv, vlibc_shutdown, plus
// vlibc_htons/vlibc_htonl/vlibc_ntohs for byte order.
//
// Only the `.ip` (AF_INET) family is bridged; `.ip6`/`.unix` fall back
// to the libc path at the call sites in tcp.c.v. DNS resolution
// (resolve_addrs), TLS/SSL, select/getpeername/close and all `$if windows`
// branches are intentionally untouched: in hosted mode libc is still
// linked, so fds created via raw syscalls keep working with it.
// setsockopt/getsockopt/getsockname/fcntl go through vlibc on Linux
// (see sockopt_d_vlibc.c.v); other vlibc targets keep the libc path.

// vlibc_ipv4_split extracts the host-order port and the network-order
// u32 address (vlibc_parse_ipv4 convention) from an AF_INET Addr.
@[inline]
fn vlibc_ipv4_split(a Addr) !(u16, u32) {
	if a.family() != .ip {
		return error('net: vlibc tcp bridge supports AF_INET only')
	}
	unsafe {
		port := vlibc_ntohs(a.addr.Ip.port)
		b := a.addr.Ip.addr
		addr := (u32(b[0]) << 24) | (u32(b[1]) << 16) | (u32(b[2]) << 8) | u32(b[3])
		return port, addr
	}
}

@[inline]
fn vlibc_wrap_err(e VlibcErrno) ! {
	if e != .enoerror {
		wrap_error(int(e))!
	}
}

// vlibc_new_tcp_socket creates a TCP socket via the vlibc socket syscall.
// The socket syscall is family-agnostic, so any family is allowed here;
// family-specific Addr layout is handled at the bind/connect call sites.
fn vlibc_new_tcp_socket(family AddrFamily) !int {
	fd, e := vlibc_socket(int(family), int(SocketType.tcp), 0)
	vlibc_wrap_err(e)!
	return int(fd)
}

// vlibc_tcp_bind binds an AF_INET socket via vlibc_bind.
fn vlibc_tcp_bind(handle int, a Addr) ! {
	port, addr := vlibc_ipv4_split(a)!
	// vlibc_bind stores sa.addr as-is, so convert the network-order value
	// to the in-memory representation first.
	vlibc_wrap_err(vlibc_bind(i64(handle), port, vlibc_htonl(addr)))!
}

// vlibc_tcp_listen marks a bound socket as listening via vlibc_listen.
fn vlibc_tcp_listen(handle int, backlog int) ! {
	vlibc_wrap_err(vlibc_listen(i64(handle), backlog))!
}

// vlibc_tcp_connect connects an AF_INET socket via vlibc_connect.
fn vlibc_tcp_connect(handle int, a Addr) ! {
	port, addr := vlibc_ipv4_split(a)!
	vlibc_wrap_err(vlibc_connect(i64(handle), port, vlibc_htonl(addr)))!
}

// vlibc_tcp_accept accepts one connection via vlibc_accept.
fn vlibc_tcp_accept(handle int) !int {
	fd, e := vlibc_accept(i64(handle))
	vlibc_wrap_err(e)!
	if fd <= 0 {
		return error('accept failed')
	}
	return int(fd)
}

// vlibc_tcp_send sends `len` bytes from `b` via vlibc_send.
fn vlibc_tcp_send(handle int, b &u8, len int, flags int) !int {
	if len <= 0 {
		return 0
	}
	data := unsafe { b.vbytes(len) }
	n, e := vlibc_send(i64(handle), data, flags)
	vlibc_wrap_err(e)!
	return int(n)
}

// vlibc_tcp_recv receives up to `len` bytes into `buf_ptr` via vlibc_recv.
fn vlibc_tcp_recv(handle int, buf_ptr &u8, len int, flags int) !int {
	if len <= 0 {
		return 0
	}
	buf := unsafe { buf_ptr.vbytes(len) }
	n, e := vlibc_recv(i64(handle), buf, flags)
	vlibc_wrap_err(e)!
	return int(n)
}

// vlibc_shutdown_result shuts down a socket via vlibc_shutdown,
// returning 0 on success or -errno (matching the C.shutdown results
// used by net.shutdown).
fn vlibc_shutdown_result(handle int, how int) int {
	e := vlibc_shutdown(i64(handle), how)
	if e == .enoerror {
		return 0
	}
	return -int(e)
}
