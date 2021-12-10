module websocket

// error_code returns the error code
fn error_code() int {
	return C.errno
}

const (
	error_ewouldblock = C.EWOULDBLOCK // blocking error code
)
