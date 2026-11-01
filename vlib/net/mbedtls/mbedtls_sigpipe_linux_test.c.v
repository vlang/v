// vtest build: linux
module mbedtls

#include <signal.h>
#include <sys/socket.h>

fn C.socketpair(domain i32, typ i32, protocol i32, sockets &i32) i32

fn test_tls_send_to_closed_peer_preserves_sigpipe_handler() {
	mut sockets := [2]i32{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	assert C.close(sockets[1]) == 0
	// Exercise the default disposition, restoring the process's handler afterward.
	previous := unsafe { C.signal(C.SIGPIPE, C.SIG_DFL) }
	defer {
		unsafe { C.signal(C.SIGPIPE, previous) }
		C.close(sockets[0])
	}
	mut context := C.mbedtls_net_context{
		fd: sockets[0]
	}
	data := [u8(1)]!
	assert C.v_mbedtls_net_send(&context, &data[0], 1) == C.MBEDTLS_ERR_NET_CONN_RESET
	assert unsafe { C.signal(C.SIGPIPE, C.SIG_DFL) } == C.SIG_DFL
}

fn test_tls_send_reports_nonblocking_backpressure() {
	mut sockets := [2]i32{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM | C.SOCK_NONBLOCK, 0, &sockets[0]) == 0
	defer {
		C.close(sockets[0])
		C.close(sockets[1])
	}
	mut context := C.mbedtls_net_context{
		fd: sockets[0]
	}
	data := [4096]u8{}
	for _ in 0 .. 1024 {
		result := C.v_mbedtls_net_send(&context, &data[0], uint(data.len))
		if result < 0 {
			assert result == C.MBEDTLS_ERR_SSL_WANT_WRITE
			return
		}
		assert result > 0
	}
	assert false, 'socket send buffer did not fill'
}

fn test_tls_send_rejects_invalid_socket() {
	mut context := C.mbedtls_net_context{
		fd: -1
	}
	data := [u8(1)]!
	assert C.v_mbedtls_net_send(&context, &data[0], 1) == C.MBEDTLS_ERR_NET_INVALID_CONTEXT
}
