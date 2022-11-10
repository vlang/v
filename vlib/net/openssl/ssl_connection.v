module openssl

import io
import net
import time
import os

// SSLConn is the current connection
pub struct SSLConn {
	config SSLConnectConfig
mut:
	sslctx   &C.SSL_CTX = unsafe { nil }
	ssl      &C.SSL     = unsafe { nil }
	handle   int
	duration time.Duration

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

// new_ssl_conn instance an new SSLCon struct
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	mut conn := &SSLConn{
		config: config
		sslctx: 0
		ssl: 0
		handle: 0
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

// shutdown closes the ssl connection and does cleanup
pub fn (mut s SSLConn) shutdown() ! {
	if s.ssl != 0 {
		mut res := 0
		for {
			res = C.SSL_shutdown(voidptr(s.ssl))
			if res < 0 {
				err_res := ssl_error(res, s.ssl) or {
					break // We break to free rest of resources
				}
				if err_res == .ssl_error_want_read {
					for {
						ready := @select(s.handle, .read, s.duration)!
						if ready {
							break
						}
					}
					continue
				} else if err_res == .ssl_error_want_write {
					for {
						ready := @select(s.handle, .write, s.duration)!
						if ready {
							break
						}
					}
					continue
				} else {
					unsafe { C.SSL_free(voidptr(s.ssl)) }
					if s.sslctx != 0 {
						C.SSL_CTX_free(s.sslctx)
					}
					return error('unexepedted ssl error $err_res')
				}
				if s.ssl != 0 {
					unsafe { C.SSL_free(voidptr(s.ssl)) }
				}
				if s.sslctx != 0 {
					C.SSL_CTX_free(s.sslctx)
				}
				return error('Could not connect using SSL. ($err_res),err')
			} else if res == 0 {
				continue
			} else if res == 1 {
				break
			}
		}
		C.SSL_free(voidptr(s.ssl))
	}
	if s.sslctx != 0 {
		C.SSL_CTX_free(s.sslctx)
	}
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

fn (mut s SSLConn) init() ! {
	s.sslctx = unsafe { C.SSL_CTX_new(C.SSLv23_client_method()) }
	if s.sslctx == 0 {
		return error("Couldn't get ssl context")
	}

	if s.config.validate {
		C.SSL_CTX_set_verify_depth(s.sslctx, 4)
		C.SSL_CTX_set_options(s.sslctx, C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3 | C.SSL_OP_NO_COMPRESSION)
	}

	s.ssl = unsafe { &C.SSL(C.SSL_new(s.sslctx)) }
	if s.ssl == 0 {
		return error("Couldn't create OpenSSL instance.")
	}

	mut res := 0

	if s.config.validate {
		mut verify := s.config.verify
		mut cert := s.config.cert
		mut cert_key := s.config.cert_key
		if s.config.in_memory_verification {
			now := time.now().unix.str()
			verify = os.temp_dir() + '/v_verify' + now
			cert = os.temp_dir() + '/v_cert' + now
			cert_key = os.temp_dir() + '/v_cert_key' + now
			if s.config.verify != '' {
				os.write_file(verify, s.config.verify)!
			}
			if s.config.cert != '' {
				os.write_file(cert, s.config.cert)!
			}
			if s.config.cert_key != '' {
				os.write_file(cert_key, s.config.cert_key)!
			}
		}
		if s.config.verify != '' {
			res = C.SSL_CTX_load_verify_locations(voidptr(s.sslctx), &char(verify.str),
				0)
			if s.config.validate && res != 1 {
				return error('http: openssl: SSL_CTX_load_verify_locations failed')
			}
		}
		if s.config.cert != '' {
			res = C.SSL_CTX_use_certificate_file(voidptr(s.sslctx), &char(cert.str), C.SSL_FILETYPE_PEM)
			if s.config.validate && res != 1 {
				return error('http: openssl: SSL_CTX_use_certificate_file failed, res: $res')
			}
		}
		if s.config.cert_key != '' {
			res = C.SSL_CTX_use_PrivateKey_file(voidptr(s.sslctx), &char(cert_key.str),
				C.SSL_FILETYPE_PEM)
			if s.config.validate && res != 1 {
				return error('http: openssl: SSL_CTX_use_PrivateKey_file failed, res: $res')
			}
		}

		preferred_ciphers := 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4'
		res = C.SSL_set_cipher_list(voidptr(s.ssl), preferred_ciphers.str)
		if s.config.validate && res != 1 {
			println('net.openssl: set cipher failed')
		}
	}
}

// connect to server using OpenSSL
pub fn (mut s SSLConn) connect(mut tcp_conn net.TcpConn, hostname string) ! {
	s.handle = tcp_conn.sock.handle
	s.duration = tcp_conn.read_timeout()

	mut res := C.SSL_set_tlsext_host_name(voidptr(s.ssl), voidptr(hostname.str))
	if res != 1 {
		return error('cannot set host name')
	}

	if C.SSL_set_fd(voidptr(s.ssl), tcp_conn.sock.handle) != 1 {
		return error("Couldn't assign ssl to socket.")
	}

	s.complete_connect() or { return err }
}

// dial opens an ssl connection on hostname:port
pub fn (mut s SSLConn) dial(hostname string, port int) ! {
	s.owns_socket = true
	mut tcp_conn := net.dial_tcp('$hostname:$port') or { return err }
	$if macos {
		tcp_conn.set_blocking(true) or { return err }
	}
	s.connect(mut tcp_conn, hostname) or { return err }
}

fn (mut s SSLConn) complete_connect() ! {
	for {
		mut res := C.SSL_connect(voidptr(s.ssl))
		if res != 1 {
			err_res := ssl_error(res, s.ssl)!
			if err_res == .ssl_error_want_read {
				for {
					ready := @select(s.handle, .read, s.duration)!
					if ready {
						break
					}
				}
				continue
			} else if err_res == .ssl_error_want_write {
				for {
					ready := @select(s.handle, .write, s.duration)!
					if ready {
						break
					}
				}
				continue
			}
			return error('Could not connect using SSL. ($err_res),err')
		}
		break
	}

	if s.config.validate {
		for {
			mut res := C.SSL_do_handshake(voidptr(s.ssl))
			if res != 1 {
				err_res := ssl_error(res, s.ssl)!
				if err_res == .ssl_error_want_read {
					for {
						ready := @select(s.handle, .read, s.duration)!
						if ready {
							break
						}
					}
					continue
				} else if err_res == .ssl_error_want_write {
					for {
						ready := @select(s.handle, .write, s.duration)!
						if ready {
							break
						}
					}
					continue
				}
				return error('Could not validate SSL certificate. ($err_res),err')
			}
			break
		}
		pcert := C.SSL_get_peer_certificate(voidptr(s.ssl))
		defer {
			if pcert != 0 {
				C.X509_free(pcert)
			}
		}
		res := C.SSL_get_verify_result(voidptr(s.ssl))
		if res != C.X509_V_OK {
			return error('SSL handshake failed')
		}
	}
}

pub fn (mut s SSLConn) socket_read_into_ptr(buf_ptr &u8, len int) !int {
	mut res := 0
	for {
		res = C.SSL_read(voidptr(s.ssl), buf_ptr, len)
		if res > 0 {
			return res
		} else if res == 0 {
			return io.Eof{}
		} else {
			err_res := ssl_error(res, s.ssl)!
			match err_res {
				.ssl_error_want_read {
					ready := @select(s.handle, .read, s.duration)!
					if !ready {
						return net.err_timed_out
					}
				}
				.ssl_error_want_write {
					ready := @select(s.handle, .write, s.duration)!
					if !ready {
						return net.err_timed_out
					}
				}
				.ssl_error_zero_return {
					return 0
				}
				else {
					return error('Could not read using SSL. ($err_res)')
				}
			}
		}
	}
	return res
}

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
			mut sent := C.SSL_write(voidptr(s.ssl), ptr, remaining)
			if sent <= 0 {
				err_res := ssl_error(sent, s.ssl)!
				if err_res == .ssl_error_want_read {
					for {
						ready := @select(s.handle, .read, s.duration)!
						if ready {
							break
						}
					}
				} else if err_res == .ssl_error_want_write {
					for {
						ready := @select(s.handle, .write, s.duration)!
						if ready {
							break
						}
					}
					continue
				} else if err_res == .ssl_error_zero_return {
					return error('ssl write on closed connection') // Todo error_with_code close
				}
				return error_with_code('Could not write SSL. ($err_res),err', int(err_res))
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
// [typedef]
// pub struct C.fd_set {
// }

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
