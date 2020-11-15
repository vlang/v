module websocket

import net

fn error_code() int {
	return C.WSAGetLastError()
}

const (
	error_ewouldblock = net.WsaError.wsaewouldblock
)
