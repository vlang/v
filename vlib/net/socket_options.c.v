module net

pub enum SocketOption {
	// TODO: SO_ACCEPT_CONN is not here because windows doesn't support it
	// and there is no easy way to define it
	broadcast = C.SO_BROADCAST
	debug = C.SO_DEBUG
	dont_route = C.SO_DONTROUTE
	error = C.SO_ERROR
	keep_alive = C.SO_KEEPALIVE
	linger = C.SO_LINGER
	oob_inline = C.SO_OOBINLINE
	reuse_addr = C.SO_REUSEADDR
	receive_buf_size = C.SO_RCVBUF
	receive_low_size = C.SO_RCVLOWAT
	receive_timeout = C.SO_RCVTIMEO
	send_buf_size = C.SO_SNDBUF
	send_low_size = C.SO_SNDLOWAT
	send_timeout = C.SO_SNDTIMEO
	socket_type = C.SO_TYPE
	ipv6_only = C.IPV6_V6ONLY
}

const (
	opts_bool    = [SocketOption.broadcast, .debug, .dont_route, .error, .keep_alive, .oob_inline]
	opts_int     = [
		.receive_buf_size,
		.receive_low_size,
		.receive_timeout,
		.send_buf_size,
		.send_low_size,
		.send_timeout,
	]

	opts_can_set = [
		SocketOption.broadcast,
		.debug,
		.dont_route,
		.keep_alive,
		.linger,
		.oob_inline,
		.receive_buf_size,
		.receive_low_size,
		.receive_timeout,
		.send_buf_size,
		.send_low_size,
		.send_timeout,
		.ipv6_only,
	]
)
