// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// ServerConn abstracts the transport connection for the HTTP/2 server,
// allowing both plain TCP (h2c) and TLS (h2) connections.
// V's structural typing means both net.TcpConn and net.mbedtls.SSLConn
// automatically satisfy this interface without explicit implementation.
pub interface ServerConn {
mut:
	read(mut buf []u8) !int
	write(data []u8) !int
	close() !
}
