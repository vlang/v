// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// BEAM backend SSL/TLS types and stubs
// These are placeholder types that allow SSL-related code to compile for the BEAM backend.
// Actual SSL operations would need to be implemented using Erlang/OTP ssl module.
module mbedtls

import io
import net
import time

// Timeout constants for BEAM backend (matching C backend)
const mbedtls_client_read_timeout_ms = $d('mbedtls_client_read_timeout_ms', 10_000)
const mbedtls_server_read_timeout_ms = $d('mbedtls_server_read_timeout_ms', 41_000)

// SSLCerts represents a pair of CA and client certificates + key
pub struct SSLCerts {
pub mut:
	cacert_data      []u8
	client_cert_data []u8
	client_key_data  []u8
}

// new_sslcerts initializes and returns a pair of SSL certificates and key
pub fn new_sslcerts() &SSLCerts {
	return &SSLCerts{}
}

// new_sslcerts_in_memory creates a pair of SSL certificates, given their contents (not paths).
pub fn new_sslcerts_in_memory(verify string, cert string, cert_key string) !&SSLCerts {
	mut certs := new_sslcerts()
	if verify != '' {
		certs.cacert_data = verify.bytes()
	}
	if cert != '' {
		certs.client_cert_data = cert.bytes()
	}
	if cert_key != '' {
		certs.client_key_data = cert_key.bytes()
	}
	return certs
}

// new_sslcerts_from_file creates a new pair of SSL certificates, given their paths on the filesystem.
pub fn new_sslcerts_from_file(verify string, cert string, cert_key string) !&SSLCerts {
	// BEAM backend: would need to read files using Erlang file I/O
	return error('net.mbedtls new_sslcerts_from_file not yet implemented for BEAM backend')
}

// cleanup frees the SSL certificates
pub fn (mut c SSLCerts) cleanup() {
	c.cacert_data = []u8{}
	c.client_cert_data = []u8{}
	c.client_key_data = []u8{}
}

// SSLConnectConfig contains configuration for SSL connections
@[params]
pub struct SSLConnectConfig {
pub:
	verify   string // the path to a rootca.pem file, containing trusted CA certificate(s)
	cert     string // the path to a cert.pem file, containing client certificate(s) for the request
	cert_key string // the path to a key.pem file, containing private keys for the client certificate(s)
	validate bool   // set this to true, if you want to stop requests, when their certificates are found to be invalid

	in_memory_verification bool // if true, verify, cert, and cert_key are read from memory, not from a file

	get_certificate ?fn (mut SSLListener, string) !&SSLCerts
}

// SSLConn is the current SSL connection
pub struct SSLConn {
pub:
	config SSLConnectConfig
pub mut:
	certs       &SSLCerts = unsafe { nil }
	handle      int
	duration    time.Duration
	opened      bool
	ip          string
	owns_socket bool
}

// SSLListener listens on a TCP port and accepts connections secured with TLS
pub struct SSLListener {
	saddr  string
	config SSLConnectConfig
mut:
	certs  &SSLCerts = unsafe { nil }
	opened bool
}

// new_ssl_listener creates a new SSLListener binding to `saddr`
pub fn new_ssl_listener(saddr string, config SSLConnectConfig) !&SSLListener {
	return error('net.mbedtls new_ssl_listener not yet implemented for BEAM backend')
}

// shutdown finishes the listener and cleans up resources
pub fn (mut l SSLListener) shutdown() ! {
	if unsafe { l.certs != nil } {
		l.certs.cleanup()
	}
	l.opened = false
}

// accept accepts a new connection and returns an SSLConn of the connected client
pub fn (mut l SSLListener) accept() !&SSLConn {
	return error('net.mbedtls SSLListener.accept not yet implemented for BEAM backend')
}

// new_ssl_conn returns a new SSLConn with the given config.
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	mut conn := &SSLConn{
		config: config
	}
	conn.init()!
	return conn
}

// close terminates the ssl connection and does cleanup
pub fn (mut s SSLConn) close() ! {
	s.shutdown()!
}

// shutdown terminates the ssl connection and does cleanup
pub fn (mut s SSLConn) shutdown() ! {
	if !s.opened {
		return error('net.mbedtls SSLConn.shutdown, connection was not open')
	}
	if unsafe { s.certs != nil } {
		s.certs.cleanup()
	}
	if s.owns_socket {
		net.shutdown(s.handle)
		net.close(s.handle)!
	}
	s.opened = false
}

// connect to server using mbedtls
fn (mut s SSLConn) init() ! {
	s.duration = 30 * time.second

	if s.config.verify != '' || s.config.cert != '' || s.config.cert_key != '' {
		s.certs = &SSLCerts{}
	}

	if s.config.in_memory_verification {
		if s.config.verify != '' {
			s.certs.cacert_data = s.config.verify.bytes()
		}
		if s.config.cert != '' {
			s.certs.client_cert_data = s.config.cert.bytes()
		}
		if s.config.cert_key != '' {
			s.certs.client_key_data = s.config.cert_key.bytes()
		}
	}
	// BEAM backend: would initialize Erlang SSL context here
}

// connect sets up an ssl connection on an existing TCP connection
pub fn (mut s SSLConn) connect(mut tcp_conn net.TcpConn, hostname string) ! {
	if s.opened {
		return error('net.mbedtls SSLConn.connect, ssl connection was already open')
	}
	s.handle = tcp_conn.sock.handle
	s.duration = 30 * time.second
	// BEAM backend: would use Erlang ssl:connect/3 here
	return error('net.mbedtls SSLConn.connect not yet implemented for BEAM backend')
}

// dial opens an ssl connection on hostname:port
pub fn (mut s SSLConn) dial(hostname string, port int) ! {
	s.owns_socket = true
	if s.opened {
		return error('net.mbedtls SSLConn.dial, the ssl connection was already open')
	}
	s.duration = 30 * time.second
	// BEAM backend: would use Erlang ssl:connect/4 here
	// Example Erlang equivalent:
	//   ssl:start(),
	//   {ok, Socket} = ssl:connect(Hostname, Port, Options)
	return error('net.mbedtls SSLConn.dial not yet implemented for BEAM backend')
}

// addr retrieves the local ip address and port number for this connection
pub fn (s &SSLConn) addr() !net.Addr {
	return net.Addr{}
}

// peer_addr retrieves the ip address and port number used by the peer
pub fn (s &SSLConn) peer_addr() !net.Addr {
	return net.Addr{}
}

// socket_read_into_ptr reads `len` bytes into `buf`
pub fn (mut s SSLConn) socket_read_into_ptr(buf_ptr &u8, len int) !int {
	// BEAM backend: would use Erlang ssl:recv/2 or ssl:recv/3 here
	return io.Eof{}
}

// read reads data from the ssl connection into `buffer`
pub fn (mut s SSLConn) read(mut buffer []u8) !int {
	return s.socket_read_into_ptr(&u8(buffer.data), buffer.len)
}

// write_ptr writes `len` bytes from `bytes` to the ssl connection
pub fn (mut s SSLConn) write_ptr(bytes &u8, len int) !int {
	// BEAM backend: would use Erlang ssl:send/2 here
	return error('net.mbedtls SSLConn.write_ptr not yet implemented for BEAM backend')
}

// write writes data from `bytes` to the ssl connection
pub fn (mut s SSLConn) write(bytes []u8) !int {
	return s.write_ptr(&u8(bytes.data), bytes.len)
}

// write_string writes a string to the ssl connection
pub fn (mut s SSLConn) write_string(str string) !int {
	return s.write_ptr(str.str, str.len)
}
