module websocket

fn error_code() int {
	return C.errno
}

const (
	error_ewouldblock = C.EWOULDBLOCK
)
