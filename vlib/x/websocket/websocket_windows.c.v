module websocket

import x.net

fn error_code() int {
	return C.WSAGetLastError()
}

const (
	error_ewouldblock = net.WsaError.wsaewouldblock
)
