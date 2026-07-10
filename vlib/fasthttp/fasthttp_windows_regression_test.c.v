// vtest build: windows
module fasthttp

fn test_accept_wrapper_preserves_pointer_sized_socket() {
	client_fd := C.v_fasthttp_accept(iocp_invalid_socket)
	assert sizeof(client_fd) == sizeof(C.SOCKET)
	assert sizeof(client_fd) == sizeof(usize)
}
