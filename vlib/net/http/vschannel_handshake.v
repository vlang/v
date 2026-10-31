module http

// Tag only handshake failures: a matching code after a write must never replay
// a request that the server may have received.
struct VSchannelHandshakeError {
	status int
}

fn (err VSchannelHandshakeError) msg() string {
	return vschannel_request_error(err.status).msg()
}

fn (err VSchannelHandshakeError) code() int {
	return err.status
}

fn vschannel_handshake_error(status int) IError {
	if status == -2146893048 { // SEC_E_INVALID_TOKEN (0x80090308)
		return VSchannelHandshakeError{ status: status }
	}
	return vschannel_request_error(status)
}

fn vschannel_handshake_retry_allowed(err IError) bool {
	return err is VSchannelHandshakeError && err.code() == -2146893048
}

fn vschannel_retry_handshake(req &Request, port int, method Method, host string, path string, data string, header Header, err IError) !Response {
	if !vschannel_handshake_retry_allowed(err) {
		return err
	}
	return net_ssl_do(req, port, method, host, path, data, header)
}
