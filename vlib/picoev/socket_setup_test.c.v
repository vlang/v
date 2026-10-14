module picoev

import net

fn test_setup_sock_accepts_tcp_socket() {
	fd := C.socket(i32(net.AddrFamily.ip), i32(net.SocketType.tcp), 0)
	assert fd != -1
	defer {
		close_socket(fd)
	}
	setup_sock(fd)!
}
