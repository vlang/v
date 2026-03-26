module v2

// ServerConn abstracts the transport connection for the HTTP/2 server.
// V's structural typing means both net.TcpConn and net.mbedtls.SSLConn
// automatically satisfy this interface without explicit implementation.
pub interface ServerConn {
mut:
	read(mut buf []u8) !int
	write(data []u8) !int
	close() !
}
