module websocket

import net

// error_code returns the error code
fn error_code() int {
	return C.WSAGetLastError()
}

const (
	error_ewouldblock = net.WsaError.wsaewouldblock // blocking error code
)
