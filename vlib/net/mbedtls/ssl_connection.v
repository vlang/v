module mbedtls

import io
import net
import time

const ctr_drbg = C.mbedtls_ctr_drbg_context{}

const entropy = C.mbedtls_entropy_context{}

fn init() {
	C.mbedtls_ctr_drbg_init(&mbedtls.ctr_drbg)
	C.mbedtls_entropy_init(&mbedtls.entropy)

	ret := C.mbedtls_ctr_drbg_seed(&mbedtls.ctr_drbg, C.mbedtls_entropy_func, &mbedtls.entropy,
		0, 0)
	if ret != 0 {
		C.mbedtls_ctr_drbg_free(&mbedtls.ctr_drbg)
		panic('Failed to seed ssl context: $ret')
	}
}

struct SSLCerts {
	cacert      C.mbedtls_x509_crt
	client_cert C.mbedtls_x509_crt
	client_key  C.mbedtls_pk_context
}

// SSLConn is the current connection
pub struct SSLConn {
	config SSLConnectConfig
mut:
	server_fd C.mbedtls_net_context
	ssl       C.mbedtls_ssl_context
	conf      C.mbedtls_ssl_config
	certs     &SSLCerts = unsafe { nil }
	handle    int
	duration  time.Duration
	opened    bool

	owns_socket bool
}

[params]
pub struct SSLConnectConfig {
	verify   string // the path to a rootca.pem file, containing trusted CA certificate(s)
	cert     string // the path to a cert.pem file, containing client certificate(s) for the request
	cert_key string // the path to a key.pem file, containing private keys for the client certificate(s)
	validate bool   // set this to true, if you want to stop requests, when their certificates are found to be invalid

	in_memory_verification bool // if true, verify, cert, and cert_key are read from memory, not from a file
}

// new_ssl_conn returns a new SSLConn with the given config.
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	mut conn := &SSLConn{
		config: config
	}
	conn.init() or { return err }
	return conn
}

// Select operation
enum Select {
	read
	write
	except
}

// shutdown terminates the ssl connection and does cleanup
pub fn (mut s SSLConn) shutdown() ! {
	if !s.opened {
		return error('ssl connection not open')
	}
	if unsafe { s.certs != nil } {
		C.mbedtls_x509_crt_free(&s.certs.cacert)
		C.mbedtls_x509_crt_free(&s.certs.client_cert)
		C.mbedtls_pk_free(&s.certs.client_key)
	}
	C.mbedtls_ssl_free(&s.ssl)
	C.mbedtls_ssl_config_free(&s.conf)
	if s.owns_socket {
		$if windows {
			C.shutdown(s.handle, C.SD_BOTH)
			net.socket_error(C.closesocket(s.handle))!
		} $else {
			C.shutdown(s.handle, C.SHUT_RDWR)
			net.socket_error(C.close(s.handle))!
		}
	}
}

// connect to server using mbedtls
fn (mut s SSLConn) init() ! {
	C.mbedtls_net_init(&s.server_fd)
	C.mbedtls_ssl_init(&s.ssl)
	C.mbedtls_ssl_config_init(&s.conf)

	mut ret := 0
	ret = C.mbedtls_ssl_config_defaults(&s.conf, C.MBEDTLS_SSL_IS_CLIENT, C.MBEDTLS_SSL_TRANSPORT_STREAM,
		C.MBEDTLS_SSL_PRESET_DEFAULT)
	if ret != 0 {
		return error_with_code('Failed to set SSL configuration', ret)
	}

	C.mbedtls_ssl_conf_rng(&s.conf, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)

	if s.config.verify != '' || s.config.cert != '' || s.config.cert_key != '' {
		s.certs = &SSLCerts{}
		C.mbedtls_x509_crt_init(&s.certs.cacert)
		C.mbedtls_x509_crt_init(&s.certs.client_cert)
		C.mbedtls_pk_init(&s.certs.client_key)
	}

	if s.config.in_memory_verification {
		if s.config.verify != '' {
			ret = C.mbedtls_x509_crt_parse(&s.certs.cacert, s.config.verify.str, s.config.verify.len)
		}
		if s.config.cert != '' {
			ret = C.mbedtls_x509_crt_parse(&s.certs.client_cert, s.config.cert.str, s.config.cert.len)
		}
		if s.config.cert_key != '' {
			ret = C.mbedtls_pk_parse_key(&s.certs.client_key, s.config.cert_key.str, s.config.cert_key.len,
				0, 0, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)
		}
	} else {
		if s.config.verify != '' {
			ret = C.mbedtls_x509_crt_parse_file(&s.certs.cacert, &char(s.config.verify.str))
		}
		if s.config.cert != '' {
			ret = C.mbedtls_x509_crt_parse_file(&s.certs.client_cert, &char(s.config.cert.str))
		}
		if s.config.cert_key != '' {
			ret = C.mbedtls_pk_parse_keyfile(&s.certs.client_key, &char(s.config.cert_key.str),
				0, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)
		}
	}
	if ret < 0 {
		return error_with_code('Failed to set certificates', ret)
	}

	if unsafe { s.certs != nil } {
		C.mbedtls_ssl_conf_ca_chain(&s.conf, &s.certs.cacert, 0)
		C.mbedtls_ssl_conf_own_cert(&s.conf, &s.certs.client_cert, &s.certs.client_key)
	}

	if s.config.validate {
		C.mbedtls_ssl_conf_authmode(&s.conf, C.MBEDTLS_SSL_VERIFY_REQUIRED)
	} else {
		C.mbedtls_ssl_conf_authmode(&s.conf, C.MBEDTLS_SSL_VERIFY_OPTIONAL)
	}

	ret = C.mbedtls_ssl_setup(&s.ssl, &s.conf)
	if ret != 0 {
		return error_with_code('Failed to setup SSL connection', ret)
	}
}

// connect sets up an ssl connection on an existing TCP connection
pub fn (mut s SSLConn) connect(mut tcp_conn net.TcpConn, hostname string) ! {
	if s.opened {
		return error('ssl connection already open')
	}
	s.handle = tcp_conn.sock.handle
	s.duration = 30 * time.second

	mut ret := C.mbedtls_ssl_set_hostname(&s.ssl, &char(hostname.str))
	if ret != 0 {
		return error_with_code('Failed to set hostname', ret)
	}

	s.server_fd.fd = s.handle

	C.mbedtls_ssl_set_bio(&s.ssl, &s.server_fd, C.mbedtls_net_send, C.mbedtls_net_recv,
		C.mbedtls_net_recv_timeout)

	ret = C.mbedtls_ssl_handshake(&s.ssl)
	if ret != 0 {
		return error_with_code('SSL handshake failed', ret)
	}

	s.opened = true
}

// dial opens an ssl connection on hostname:port
pub fn (mut s SSLConn) dial(hostname string, port int) ! {
	s.owns_socket = true
	if s.opened {
		return error('ssl connection already open')
	}
	s.duration = 30 * time.second

	mut ret := C.mbedtls_ssl_set_hostname(&s.ssl, &char(hostname.str))
	if ret != 0 {
		return error_with_code('Failed to set hostname', ret)
	}

	port_str := port.str()
	ret = C.mbedtls_net_connect(&s.server_fd, &char(hostname.str), &char(port_str.str),
		C.MBEDTLS_NET_PROTO_TCP)
	if ret != 0 {
		return error_with_code('Failed to connect to host', ret)
	}

	C.mbedtls_ssl_set_bio(&s.ssl, &s.server_fd, C.mbedtls_net_send, C.mbedtls_net_recv,
		C.mbedtls_net_recv_timeout)

	s.handle = s.server_fd.fd

	ret = C.mbedtls_ssl_handshake(&s.ssl)
	if ret != 0 {
		return error_with_code('SSL handshake failed', ret)
	}

	s.opened = true
}

// socket_read_into_ptr reads `len` bytes into `buf`
pub fn (mut s SSLConn) socket_read_into_ptr(buf_ptr &u8, len int) !int {
	mut res := 0
	for {
		res = C.mbedtls_ssl_read(&s.ssl, buf_ptr, len)
		if res > 0 {
			return res
		} else if res == 0 {
			return io.Eof{}
		} else {
			match res {
				C.MBEDTLS_ERR_SSL_WANT_READ {
					ready := @select(s.handle, .read, s.duration)!
					if !ready {
						return net.err_timed_out
					}
				}
				C.MBEDTLS_ERR_SSL_WANT_WRITE {
					ready := @select(s.handle, .write, s.duration)!
					if !ready {
						return net.err_timed_out
					}
				}
				C.MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY {
					break
				}
				else {
					return error_with_code('Could not read using SSL', res)
				}
			}
		}
	}
	return res
}

// read reads data from the ssl connection into `buffer`
pub fn (mut s SSLConn) read(mut buffer []u8) !int {
	res := s.socket_read_into_ptr(&u8(buffer.data), buffer.len) or { return err }
	return res
}

// write_ptr writes `len` bytes from `bytes` to the ssl connection
pub fn (mut s SSLConn) write_ptr(bytes &u8, len int) !int {
	unsafe {
		mut ptr_base := bytes
		mut total_sent := 0
		for total_sent < len {
			ptr := ptr_base + total_sent
			remaining := len - total_sent
			mut sent := C.mbedtls_ssl_write(&s.ssl, ptr, remaining)
			if sent <= 0 {
				match sent {
					C.MBEDTLS_ERR_SSL_WANT_READ {
						for {
							ready := @select(s.handle, .read, s.duration)!
							if ready {
								break
							}
						}
						continue
					}
					C.MBEDTLS_ERR_SSL_WANT_WRITE {
						for {
							ready := @select(s.handle, .write, s.duration)!
							if ready {
								break
							}
						}
						continue
					}
					else {
						return error_with_code('Could not write using SSL', sent)
					}
				}
			}
			total_sent += sent
		}
		return total_sent
	}
}

// write writes data from `bytes` to the ssl connection
pub fn (mut s SSLConn) write(bytes []u8) !int {
	return s.write_ptr(&u8(bytes.data), bytes.len)
}

// write_string writes a string to the ssl connection
pub fn (mut s SSLConn) write_string(str string) !int {
	return s.write_ptr(str.str, str.len)
}

/*
This is basically a copy of Emily socket implementation of select.
	This have to be consolidated into common net lib features
	when merging this to V
*/

// Select waits for an io operation (specified by parameter `test`) to be available
fn @select(handle int, test Select, timeout time.Duration) !bool {
	set := C.fd_set{}

	C.FD_ZERO(&set)
	C.FD_SET(handle, &set)

	seconds := timeout.milliseconds() / 1000
	microseconds := timeout - (seconds * time.second)
	mut tt := C.timeval{
		tv_sec: u64(seconds)
		tv_usec: u64(microseconds)
	}

	mut timeval_timeout := &tt

	// infinite timeout is signaled by passing null as the timeout to
	// select
	if timeout == net.infinite_timeout {
		timeval_timeout = &C.timeval(0)
	}

	match test {
		.read {
			net.socket_error(C.@select(handle + 1, &set, C.NULL, C.NULL, timeval_timeout))!
		}
		.write {
			net.socket_error(C.@select(handle + 1, C.NULL, &set, C.NULL, timeval_timeout))!
		}
		.except {
			net.socket_error(C.@select(handle + 1, C.NULL, C.NULL, &set, timeval_timeout))!
		}
	}

	return C.FD_ISSET(handle, &set)
}
