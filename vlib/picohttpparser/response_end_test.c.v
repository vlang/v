module picohttpparser

// Exercise C.send even when this module is built independently of net.
// The cached module object must declare send and link Winsock on Windows.
fn test_response_end_invalid_socket_returns_error() {
	mut payload := [u8(`x`)]
	mut response := Response{
		fd:        -1
		buf_start: unsafe { &payload[0] }
		buf:       unsafe { &payload[0] + 1 }
	}
	assert response.end() == -1
}
