module openssl

import io
import net
import time
import os

// SSLConn is the current connection
pub struct SSLConn {
pub:
	config SSLConnectConfig
pub mut:
	sslctx   &C.SSL_CTX = unsafe { nil }
	ssl      &C.SSL     = unsafe { nil }
	handle   int
	duration time.Duration

	owns_socket bool
}

@[params]
pub struct SSLConnectConfig {
pub:
	verify   string // the path to a rootca.pem file, containing trusted CA certificate(s)
	cert     string // the path to a cert.pem file, containing client certificate(s) for the request
	cert_key string // the path to a key.pem file, containing private keys for the client certificate(s)
	validate bool   // set this to true, if you want to stop requests, when their certificates are found to be invalid

	in_memory_verification bool // if true, verify, cert, and cert_key are read from memory, not from a file
}

// new_ssl_conn instance an new SSLCon struct
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}
	mut conn := &SSLConn{
		config: config
		sslctx: unsafe { nil }
		ssl:    unsafe { nil }
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

// close closes the ssl connection and does cleanup
pub fn (mut s SSLConn) close() ! {
	s.shutdown()!
}

// shutdown closes the ssl connection and does cleanup
pub fn (mut s SSLConn) shutdown() ! {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}

	if s.ssl != 0 {
		deadline := time.now().add(s.duration)
		for {
			mut res := C.SSL_shutdown(voidptr(s.ssl))
			if res == 1 {
				break
			}

			err_res := ssl_error(res, s.ssl) or {
				break // We break to free rest of resources
			}
			if err_res == .ssl_error_want_read {
				s.wait_for_read(deadline - time.now())!
				continue
			} else if err_res == .ssl_error_want_write {
				s.wait_for_write(deadline - time.now())!
				continue
			}
			if s.ssl != 0 {
				unsafe { C.SSL_free(voidptr(s.ssl)) }
			}
			if s.sslctx != 0 {
				C.SSL_CTX_free(s.sslctx)
			}
			return error('Could not connect using SSL. (${err_res}),err')
		}
		C.SSL_free(voidptr(s.ssl))
	}
	if s.sslctx != 0 {
		C.SSL_CTX_free(s.sslctx)
	}
	if s.owns_socket {
		net.shutdown(s.handle)
		net.close(s.handle)!
	}
}

fn (mut s SSLConn) init() ! {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}
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
			now := time.now().unix().str()
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
				return error('http: openssl: SSL_CTX_use_certificate_file failed, res: ${res}')
			}
		}
		if s.config.cert_key != '' {
			res = C.SSL_CTX_use_PrivateKey_file(voidptr(s.sslctx), &char(cert_key.str),
				C.SSL_FILETYPE_PEM)
			if s.config.validate && res != 1 {
				return error('http: openssl: SSL_CTX_use_PrivateKey_file failed, res: ${res}')
			}
		}

		preferred_ciphers := 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4'
		res = C.SSL_set_cipher_list(voidptr(s.ssl), &char(preferred_ciphers.str))
		if s.config.validate && res != 1 {
			println('net.openssl: set cipher failed')
		}
	}
}

// connect to server using OpenSSL
pub fn (mut s SSLConn) connect(mut tcp_conn net.TcpConn, hostname string) ! {
	$if trace_ssl ? {
		eprintln('${@METHOD} hostname: ${hostname}')
	}
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
	$if trace_ssl ? {
		eprintln('${@METHOD} hostname: ${hostname} | port: ${port}')
	}
	s.owns_socket = true
	mut tcp_conn := net.dial_tcp('${hostname}:${port}') or { return err }
	s.connect(mut tcp_conn, hostname) or { return err }
}

fn (mut s SSLConn) complete_connect() ! {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}

	deadline := time.now().add(s.duration)
	for {
		mut res := C.SSL_connect(voidptr(s.ssl))
		if res == 1 {
			break
		}

		err_res := ssl_error(res, s.ssl)!
		if err_res == .ssl_error_want_read {
			s.wait_for_read(deadline - time.now())!
			continue
		}
		if err_res == .ssl_error_want_write {
			s.wait_for_write(deadline - time.now())!
			continue
		}
		return error('Could not connect using SSL. (${err_res}),err')
	}

	if s.config.validate {
		mut pcert := &C.X509(unsafe { nil })
		for {
			mut res := C.SSL_do_handshake(voidptr(s.ssl))
			if res == 1 {
				break
			}

			err_res := ssl_error(res, s.ssl)!
			if err_res == .ssl_error_want_read {
				s.wait_for_read(deadline - time.now())!
				continue
			} else if err_res == .ssl_error_want_write {
				s.wait_for_write(deadline - time.now())!
				continue
			}
			return error('Could not validate SSL certificate. (${err_res}),err')
		}
		$if openbsd {
			pcert = C.SSL_get_peer_certificate(voidptr(s.ssl))
		} $else {
			pcert = C.SSL_get1_peer_certificate(voidptr(s.ssl))
		}
		defer {
			if pcert != 0 {
				C.X509_free(pcert)
			}
		}
		res := C.SSL_get_verify_result(voidptr(s.ssl))
		if res != C.X509_V_OK {
			return error('SSL handshake failed (OpenSSL SSL_get_verify_result = ${res})')
		}
	}
}

// addr retrieves the local ip address and port number for this connection
pub fn (s &SSLConn) addr() !net.Addr {
	return net.addr_from_socket_handle(s.handle)
}

// peer_addr retrieves the ip address and port number used by the peer
pub fn (s &SSLConn) peer_addr() !net.Addr {
	return net.peer_addr_from_socket_handle(s.handle)
}

pub fn (mut s SSLConn) socket_read_into_ptr(buf_ptr &u8, len int) !int {
	mut res := 0
	$if trace_ssl ? {
		defer {
			if len > 0 {
				eprintln('${@METHOD} res: ${res}: buf_ptr: ${voidptr(buf_ptr):x}, len: ${len}, hex: ${unsafe { buf_ptr.vbytes(len).hex() }} data: `${unsafe { buf_ptr.vstring_with_len(len) }}`')
			}
		}
	}

	deadline := time.now().add(s.duration)
	// s.wait_for_read(deadline - time.now())!
	for {
		res = C.SSL_read(voidptr(s.ssl), buf_ptr, len)
		if res > 0 {
			return res
		} else if res == 0 {
			$if trace_ssl ? {
				eprintln('${@METHOD} ---> res: io.Eof')
			}
			return io.Eof{}
		} else {
			err_res := ssl_error(res, s.ssl)!
			match err_res {
				.ssl_error_want_read {
					s.wait_for_read(deadline - time.now()) or {
						$if trace_ssl ? {
							eprintln('${@METHOD} ---> res: ${err} .ssl_error_want_read')
						}
						return err
					}
				}
				.ssl_error_want_write {
					s.wait_for_write(deadline - time.now()) or {
						$if trace_ssl ? {
							eprintln('${@METHOD} ---> res: ${err} .ssl_error_want_write')
						}
						return err
					}
				}
				.ssl_error_zero_return {
					$if trace_ssl ? {
						eprintln('${@METHOD} ---> res: 0 .ssl_error_zero_return')
					}
					return 0
				}
				else {
					$if trace_ssl ? {
						eprintln('${@METHOD} ---> res: could not read, err_res: ${err_res}')
					}
					return error('Could not read using SSL. (${err_res})')
				}
			}
		}
	}

	// Dead code, for the compiler to pass
	return error('Unknown error')
}

pub fn (mut s SSLConn) read(mut buffer []u8) !int {
	$if trace_ssl ? {
		eprintln('${@METHOD} buffer.len: ${buffer.len}')
	}
	return s.socket_read_into_ptr(&u8(buffer.data), buffer.len)
}

// write_ptr writes `len` bytes from `bytes` to the ssl connection
pub fn (mut s SSLConn) write_ptr(bytes &u8, len int) !int {
	mut total_sent := 0
	$if trace_ssl ? {
		defer {
			eprintln('${@METHOD} total_sent: ${total_sent}, bytes: ${voidptr(bytes):x}, len: ${len}, hex: ${unsafe { bytes.vbytes(len).hex() }}, data:-=-=-=-\n${unsafe { bytes.vstring_with_len(len) }}\n-=-=-=-')
		}
	}

	deadline := time.now().add(s.duration)
	unsafe {
		mut ptr_base := bytes
		for total_sent < len {
			ptr := ptr_base + total_sent
			remaining := len - total_sent
			mut sent := C.SSL_write(voidptr(s.ssl), ptr, remaining)
			if sent <= 0 {
				err_res := ssl_error(sent, s.ssl)!
				if err_res == .ssl_error_want_read {
					s.wait_for_read(deadline - time.now())!
					continue
				} else if err_res == .ssl_error_want_write {
					s.wait_for_write(deadline - time.now())!
					continue
				} else if err_res == .ssl_error_zero_return {
					$if trace_ssl ? {
						eprintln('${@METHOD} ---> res: ssl write on closed connection .ssl_error_zero_return')
					}
					return error('ssl write on closed connection') // TODO: error_with_code close
				}
				$if trace_ssl ? {
					eprintln('${@METHOD} ---> res: could not write SSL, err_res: ${err_res}')
				}
				return error_with_code('Could not write SSL. (${err_res}),err', int(err_res))
			}
			total_sent += sent
		}
	}
	return total_sent
}

// write writes data from `bytes` to the ssl connection
pub fn (mut s SSLConn) write(bytes []u8) !int {
	return s.write_ptr(&u8(bytes.data), bytes.len)!
}

// write_string writes a string to the ssl connection
pub fn (mut s SSLConn) write_string(str string) !int {
	$if trace_ssl ? {
		eprintln('${@METHOD} str: ${str}')
	}
	return s.write_ptr(str.str, str.len)
}

// Select waits for an io operation (specified by parameter `test`) to be available
fn select(handle int, test Select, timeout time.Duration) !bool {
	$if trace_ssl ? {
		eprintln('${@METHOD} handle: ${handle}, timeout: ${timeout}')
	}
	set := C.fd_set{}
	C.FD_ZERO(&set)
	C.FD_SET(handle, &set)

	deadline := time.now().add(timeout)
	mut remaining_time := timeout.milliseconds()
	for remaining_time > 0 {
		seconds := remaining_time / 1000
		microseconds := (remaining_time % 1000) * 1000

		tt := C.timeval{
			tv_sec:  u64(seconds)
			tv_usec: u64(microseconds)
		}
		timeval_timeout := if timeout < 0 {
			&C.timeval(unsafe { nil })
		} else {
			&tt
		}

		mut res := -1
		match test {
			.read {
				res = net.socket_error(C.select(handle + 1, &set, C.NULL, C.NULL, timeval_timeout))!
			}
			.write {
				res = net.socket_error(C.select(handle + 1, C.NULL, &set, C.NULL, timeval_timeout))!
			}
			.except {
				res = net.socket_error(C.select(handle + 1, C.NULL, C.NULL, &set, timeval_timeout))!
			}
		}
		if res < 0 {
			if C.errno == C.EINTR {
				// errno is 4, Spurious wakeup from signal, keep waiting
				remaining_time = (deadline - time.now()).milliseconds()
				continue
			}
			return error_with_code('Select failed: ${res}', C.errno)
		} else if res == 0 {
			return net.err_timed_out
		}

		res = C.FD_ISSET(handle, &set)
		$if trace_ssl ? {
			eprintln('${@METHOD} ---> res: ${res}')
		}
		return res != 0
	}

	return net.err_timed_out
}

// wait_for wraps the common wait code
fn wait_for(handle int, what Select, timeout time.Duration) ! {
	ready := select(handle, what, timeout)!
	if ready {
		return
	}

	return net.err_timed_out
}

// wait_for_write waits for a write io operation to be available
fn (mut s SSLConn) wait_for_write(timeout time.Duration) ! {
	return wait_for(s.handle, .write, timeout)
}

// wait_for_read waits for a read io operation to be available
fn (mut s SSLConn) wait_for_read(timeout time.Duration) ! {
	return wait_for(s.handle, .read, timeout)
}
