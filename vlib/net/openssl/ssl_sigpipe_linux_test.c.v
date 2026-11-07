// vtest build: linux && present_openssl?
module openssl

#include <signal.h>
#include <sys/socket.h>

fn C.socketpair(domain i32, typ i32, protocol i32, sockets &i32) i32

fn C.SSL_get_wbio(ssl &C.SSL) &C.BIO

fn C.BIO_write(bio &C.BIO, data voidptr, len i32) i32

fn C.BIO_should_write(bio &C.BIO) int

fn test_ssl_socket_bio_preserves_sigpipe_handler_and_borrowed_socket() ! {
	mut sockets := [2]i32{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	assert C.close(sockets[1]) == 0
	// The default disposition makes an unsuppressed socket write terminate the test.
	previous := unsafe { C.signal(C.SIGPIPE, C.SIG_DFL) }
	defer {
		unsafe { C.signal(C.SIGPIPE, previous) }
		C.close(sockets[0])
	}
	mut conn := new_ssl_conn(SSLConnectConfig{})!
	assert C.v_net_openssl_SSL_set_fd(conn.ssl, sockets[0]) == 1
	data := [u8(1)]!
	assert C.BIO_write(C.SSL_get_wbio(conn.ssl), &data[0], 1) == -1
	assert C.BIO_should_write(C.SSL_get_wbio(conn.ssl)) == 0
	assert unsafe { C.signal(C.SIGPIPE, C.SIG_DFL) } == C.SIG_DFL
	conn.shutdown()!
	assert C.fcntl(sockets[0], C.F_GETFD) >= 0
}

fn test_ssl_socket_bio_reports_nonblocking_backpressure() ! {
	mut sockets := [2]i32{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM | C.SOCK_NONBLOCK, 0, &sockets[0]) == 0
	mut conn := new_ssl_conn(SSLConnectConfig{})!
	defer {
		conn.shutdown() or {}
		C.close(sockets[0])
		C.close(sockets[1])
	}
	assert C.v_net_openssl_SSL_set_fd(conn.ssl, sockets[0]) == 1
	bio := C.SSL_get_wbio(conn.ssl)
	data := [4096]u8{}
	for _ in 0 .. 1024 {
		result := C.BIO_write(bio, &data[0], data.len)
		if result < 0 {
			assert C.BIO_should_write(bio) != 0
			return
		}
		assert result > 0
	}
	assert false, 'socket send buffer did not fill'
}
