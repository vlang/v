module openssl

import net.openssl
import net
import time

// const (
// is_used = openssl.is_used
// )
pub struct SSLConn {
mut:
	sslctx   &C.SSL_CTX
	ssl      &C.SSL
	handle   int
	duration time.Duration
}

enum Select {
	read
	write
	except
}

pub fn new_ssl_conn() &SSLConn {
	return &SSLConn{
		sslctx: 0
		ssl: 0
		handle: 0
	}
}

// shutdown closes the ssl connection and do clean up
pub fn (mut s SSLConn) shutdown() ? {
	if s.ssl != 0 {
		mut res := 0
		for {
			res = C.SSL_shutdown(s.ssl)
			if res < 0 {
				err_res := openssl.ssl_error(res, s.ssl) or {
					break // We break to free rest of resources
				}
				if err_res == .ssl_error_want_read {
					for {
						ready := @select(s.handle, .read, s.duration)?
						if ready {
							break
						}
					}
					continue
				} else if err_res == .ssl_error_want_write {
					for {
						ready := @select(s.handle, .write, s.duration)?
						if ready {
							break
						}
					}
					continue
				} else {
					C.SSL_free(s.ssl)
					if s.sslctx != 0 {
						C.SSL_CTX_free(s.sslctx)
					}
					return error('unexepedted ssl error $err_res')
				}
				if s.ssl != 0 {
					C.SSL_free(s.ssl)
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
		C.SSL_free(s.ssl)
	}
	if s.sslctx != 0 {
		C.SSL_CTX_free(s.sslctx)
	}
}

// connect to server using open ssl
pub fn (mut s SSLConn) connect(mut tcp_conn net.TcpConn, hostname string) ? {
	s.handle = tcp_conn.sock.handle
	s.duration = tcp_conn.read_timeout()

	s.sslctx = C.SSL_CTX_new(C.SSLv23_client_method())
	if s.sslctx == 0 {
		return error("Couldn't get ssl context")
	}

	// TODO: Fix option to enable/disable checks for valid
	//		 certificates to allow both secure and self signed
	//		 for now the checks are not done at all to comply 
	// 		 to current autobahn tests

	// C.SSL_CTX_set_verify_depth(s.sslctx, 4)
	// flags := C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3 | C.SSL_OP_NO_COMPRESSION
	// C.SSL_CTX_set_options(s.sslctx, flags)
	// mut res := C.SSL_CTX_load_verify_locations(s.sslctx, 'random-org-chain.pem', 0)
	
	s.ssl = C.SSL_new(s.sslctx)
	if s.ssl == 0 {
		return error("Couldn't create OpenSSL instance.")
	}

	// preferred_ciphers := 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4'
	// mut res := C.SSL_set_cipher_list(s.ssl, preferred_ciphers.str)
	// if res != 1 {
	// 	println('http: openssl: cipher failed')
	// }
	
	mut res := C.SSL_set_tlsext_host_name(s.ssl, hostname.str)
	if res != 1 {
		return error('cannot set host name')
	}

	if C.SSL_set_fd(s.ssl, tcp_conn.sock.handle) != 1 {
		return error("Couldn't assign ssl to socket.")
	}
	for {
		res = C.SSL_connect(s.ssl)
		if res != 1 {
			err_res := openssl.ssl_error(res, s.ssl)?
			if err_res == .ssl_error_want_read {
				for {
					ready := @select(s.handle, .read, s.duration)?
					if ready {
						break
					}
				}
				continue
			} else if err_res == .ssl_error_want_write {
				for {
					ready := @select(s.handle, .write, s.duration)?
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
}

pub fn (mut s SSLConn) socket_read_into_ptr(buf_ptr byteptr, len int) ?int {
	mut res := 0
	for {
		res = C.SSL_read(s.ssl, buf_ptr, len)
		if res < 0 {
			err_res := openssl.ssl_error(res, s.ssl)?
			if err_res == .ssl_error_want_read {
				for {
					ready := @select(s.handle, .read, s.duration)?
					if ready {
						break
					}
				}
				continue
			} else if err_res == .ssl_error_want_write {
				for {
					ready := @select(s.handle, .write, s.duration)?
					if ready {
						break
					}
				}
				continue
			} else if err_res == .ssl_error_zero_return {
				return 0
			}
			return error('Could not read using SSL. ($err_res)')
		}
		break
	}
	return res
}

pub fn (mut s SSLConn) read_into(mut buffer []byte) ?int {
	res := s.socket_read_into_ptr(byteptr(buffer.data), buffer.len)?
	return res
}

// write number of bytes to SSL connection
pub fn (mut s SSLConn) write(bytes []byte) ? {
	unsafe {
		mut ptr_base := byteptr(bytes.data)
		mut total_sent := 0
		for total_sent < bytes.len {
			ptr := ptr_base + total_sent
			remaining := bytes.len - total_sent
			mut sent := C.SSL_write(s.ssl, ptr, remaining)
			if sent <= 0 {
				err_res := openssl.ssl_error(sent, s.ssl)?
				if err_res == .ssl_error_want_read {
					for {
						ready := @select(s.handle, .read, s.duration) ?
						if ready {
							break
						}
					}
				} else if err_res == .ssl_error_want_write {
					for {
						ready := @select(s.handle, .write, s.duration)?
						if ready {
							break
						}
					}
					continue
				} else if err_res == .ssl_error_zero_return {
					return error('ssl write on closed connection') // Todo error_with_code close
				}
				return error_with_code('Could not write SSL. ($err_res),err', err_res)
			}
			total_sent += sent
		}
	}
}

/*
This is basically a copy of Emily socket implementation of select.
	This have to be consolidated into common net lib features
	when merging this to V
*/
[typedef]
pub struct C.fd_set {
}

// Select waits for an io operation (specified by parameter `test`) to be available
fn @select(handle int, test Select, timeout time.Duration) ?bool {
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
            net.socket_error(C.@select(handle+1, &set, C.NULL, C.NULL, timeval_timeout))?
        }
        .write {
            net.socket_error(C.@select(handle+1, C.NULL, &set, C.NULL, timeval_timeout))?
        }
        .except {
            net.socket_error(C.@select(handle+1, C.NULL, C.NULL, &set, timeval_timeout))?
        }
    }

    return C.FD_ISSET(handle, &set)
}
