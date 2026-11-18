module veb

import net
import time

// takeover_live_pair returns a connected listener/accepted/dialer triple. The
// accepted (server-side) socket emulates what the fasthttp backends hand to
// takeover: a non-blocking client fd (accept4(SOCK_NONBLOCK) on Linux,
// set_nonblocking right after accept on BSD).
fn takeover_live_pair() (&net.TcpListener, &net.TcpConn, &net.TcpConn) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0') or { panic(err) }
	address := listener.addr() or {
		listener.close() or {}
		panic(err)
	}
	mut dialer := net.dial_tcp(address.str()) or {
		listener.close() or {}
		panic(err)
	}
	mut accepted := listener.accept() or {
		dialer.close() or {}
		listener.close() or {}
		panic(err)
	}
	accepted.set_blocking(false) or { panic(err) }
	return listener, accepted, dialer
}

// test_takeover_conn_leaves_the_socket_non_blocking guards vlang/v#32329: the
// write timeout only applies when send() reports it would block, which never
// happens on a blocking socket, so a stalled client would keep the writer
// blocked indefinitely. Takeover must leave the client socket non-blocking.
fn test_takeover_conn_leaves_the_socket_non_blocking() {
	listener, accepted, mut dialer := takeover_live_pair()
	defer {
		listener.close() or {}
		dialer.close() or {}
	}
	fd := accepted.handle
	mut ctx := Context{
		client_fd: fd
	}
	ctx.takeover_conn()
	assert ctx.takeover_mode == .manual
	assert ctx.conn != unsafe { nil }
	assert ctx.conn.is_blocking == false
	assert ctx.conn.read_timeout == 30 * time.second
	assert ctx.conn.write_timeout == 30 * time.second
	$if !windows {
		flags := C.fcntl(fd, C.F_GETFL, 0)
		assert flags != -1
		assert flags & C.O_NONBLOCK != 0, 'takeover must leave the client socket non-blocking, so the write timeout applies'
	}
	// `accepted` still points at the same fd; close it exactly once, via ctx.conn.
	ctx.conn.close() or {}
}

// test_takeover_conn_switches_an_existing_conn_to_non_blocking covers the
// second prepare_takeover_conn branch: a conn that arrived in blocking mode
// (e.g. from listener.accept()) must be normalized to non-blocking as well.
fn test_takeover_conn_switches_an_existing_conn_to_non_blocking() {
	listener, mut accepted, mut dialer := takeover_live_pair()
	defer {
		listener.close() or {}
		dialer.close() or {}
	}
	accepted.set_blocking(true) or { panic(err) }
	mut ctx := Context{
		conn: accepted
	}
	ctx.takeover_conn()
	assert ctx.takeover_mode == .manual
	assert ctx.conn.is_blocking == false
	assert ctx.conn.read_timeout == 30 * time.second
	assert ctx.conn.write_timeout == 30 * time.second
	$if !windows {
		flags := C.fcntl(accepted.handle, C.F_GETFL, 0)
		assert flags != -1
		assert flags & C.O_NONBLOCK != 0, 'takeover must switch an existing conn to non-blocking, so the write timeout applies'
	}
	accepted.close() or {}
}
