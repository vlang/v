module mbedtls

import io
import net
import time

const ctr_drbg = C.mbedtls_ctr_drbg_context{}

const entropy = C.mbedtls_entropy_context{}

fn init() {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}
	unsafe { // Unsafe is needed for taking an address of const
		C.mbedtls_ctr_drbg_init(&mbedtls.ctr_drbg)
		C.mbedtls_entropy_init(&mbedtls.entropy)
		ret := C.mbedtls_ctr_drbg_seed(&mbedtls.ctr_drbg, C.mbedtls_entropy_func, &mbedtls.entropy,
			0, 0)
		if ret != 0 {
			C.mbedtls_ctr_drbg_free(&mbedtls.ctr_drbg)
			panic('Failed to seed ssl context: ${ret}')
		}
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

// SSLListener listens on a TCP port and accepts connection secured with TLS
pub struct SSLListener {
	saddr  string
	config SSLConnectConfig
mut:
	server_fd C.mbedtls_net_context
	ssl       C.mbedtls_ssl_context
	conf      C.mbedtls_ssl_config
	certs     &SSLCerts = unsafe { nil }
	opened    bool
	// handle		int
	// duration	time.Duration
}

// create a new SSLListener binding to `saddr`
pub fn new_ssl_listener(saddr string, config SSLConnectConfig) !&SSLListener {
	mut listener := &SSLListener{
		saddr: saddr
		config: config
	}
	listener.init()!
	listener.opened = true
	return listener
}

// finish the listener and clean up resources
pub fn (mut l SSLListener) shutdown() ! {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}
	if unsafe { l.certs != nil } {
		C.mbedtls_x509_crt_free(&l.certs.cacert)
		C.mbedtls_x509_crt_free(&l.certs.client_cert)
		C.mbedtls_pk_free(&l.certs.client_key)
	}
	C.mbedtls_ssl_free(&l.ssl)
	C.mbedtls_ssl_config_free(&l.conf)
	if l.opened {
		C.mbedtls_net_free(&l.server_fd)
	}
}

// internal function to init and bind the listener
fn (mut l SSLListener) init() ! {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}

	lhost, lport := net.split_address(l.saddr)!
	if l.config.cert == '' || l.config.cert_key == '' {
		return error('No certificate or key provided')
	}
	if l.config.validate && l.config.verify == '' {
		return error('No root CA provided')
	}
	C.mbedtls_net_init(&l.server_fd)
	C.mbedtls_ssl_init(&l.ssl)
	C.mbedtls_ssl_config_init(&l.conf)
	l.certs = &SSLCerts{}
	C.mbedtls_x509_crt_init(&l.certs.client_cert)
	C.mbedtls_pk_init(&l.certs.client_key)

	unsafe {
		C.mbedtls_ssl_conf_rng(&l.conf, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)
	}

	mut ret := 0

	if l.config.in_memory_verification {
		if l.config.verify != '' {
			ret = C.mbedtls_x509_crt_parse(&l.certs.cacert, l.config.verify.str,
				l.config.verify.len + 1)
		}
		if l.config.cert != '' {
			ret = C.mbedtls_x509_crt_parse(&l.certs.client_cert, l.config.cert.str,
				l.config.cert.len + 1)
		}
		if l.config.cert_key != '' {
			unsafe {
				ret = C.mbedtls_pk_parse_key(&l.certs.client_key, l.config.cert_key.str,
					l.config.cert_key.len + 1, 0, 0, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)
			}
		}
	} else {
		if l.config.verify != '' {
			ret = C.mbedtls_x509_crt_parse_file(&l.certs.cacert, &char(l.config.verify.str))
		}
		ret = C.mbedtls_x509_crt_parse_file(&l.certs.client_cert, &char(l.config.cert.str))
		unsafe {
			ret = C.mbedtls_pk_parse_keyfile(&l.certs.client_key, &char(l.config.cert_key.str),
				0, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)
		}
	}

	if l.config.validate {
		C.mbedtls_ssl_conf_authmode(&l.conf, C.MBEDTLS_SSL_VERIFY_REQUIRED)
	}

	mut bind_ip := unsafe { nil }
	if lhost != '' {
		bind_ip = voidptr(lhost.str)
	}
	bind_port := lport.str()

	ret = C.mbedtls_net_bind(&l.server_fd, bind_ip, voidptr(bind_port.str), C.MBEDTLS_NET_PROTO_TCP)

	if ret != 0 {
		return error_with_code("can't bind to ${l.saddr}", ret)
	}

	ret = C.mbedtls_ssl_config_defaults(&l.conf, C.MBEDTLS_SSL_IS_SERVER, C.MBEDTLS_SSL_TRANSPORT_STREAM,
		C.MBEDTLS_SSL_PRESET_DEFAULT)
	if ret != 0 {
		return error_with_code("can't to set config defaults", ret)
	}

	C.mbedtls_ssl_conf_ca_chain(&l.conf, &l.certs.cacert, unsafe { nil })
	ret = C.mbedtls_ssl_conf_own_cert(&l.conf, &l.certs.client_cert, &l.certs.client_key)

	if ret != 0 {
		return error_with_code("can't load certificate", ret)
	}

	ret = C.mbedtls_ssl_setup(&l.ssl, &l.conf)

	if ret != 0 {
		return error_with_code("can't setup ssl", ret)
	}
}

// accepts a new connection and returns a SSLConn of the connected client
pub fn (mut l SSLListener) accept() !&SSLConn {
	mut conn := &SSLConn{
		config: l.config
		opened: true
	}

	// TODO: save the client's IP address somewhere (maybe add a field to SSLConn ?)
	mut ret := C.mbedtls_net_accept(&l.server_fd, &conn.server_fd, unsafe { nil }, 0,
		unsafe { nil })
	if ret != 0 {
		return error_with_code("can't accept connection", ret)
	}
	conn.handle = conn.server_fd.fd
	conn.owns_socket = true

	C.mbedtls_ssl_init(&conn.ssl)
	C.mbedtls_ssl_config_init(&conn.conf)
	ret = C.mbedtls_ssl_setup(&conn.ssl, &l.conf)

	if ret != 0 {
		return error_with_code('SSL setup failed', ret)
	}

	C.mbedtls_ssl_set_bio(&conn.ssl, &conn.server_fd, C.mbedtls_net_send, C.mbedtls_net_recv,
		unsafe { nil })

	ret = C.mbedtls_ssl_handshake(&conn.ssl)
	for ret != 0 {
		if ret != C.MBEDTLS_ERR_SSL_WANT_READ && ret != C.MBEDTLS_ERR_SSL_WANT_WRITE {
			return error_with_code('SSL handshake failed', ret)
		}
		ret = C.mbedtls_ssl_handshake(&conn.ssl)
	}

	return conn
}

@[params]
pub struct SSLConnectConfig {
	verify   string // the path to a rootca.pem file, containing trusted CA certificate(s)
	cert     string // the path to a cert.pem file, containing client certificate(s) for the request
	cert_key string // the path to a key.pem file, containing private keys for the client certificate(s)
	validate bool   // set this to true, if you want to stop requests, when their certificates are found to be invalid

	in_memory_verification bool // if true, verify, cert, and cert_key are read from memory, not from a file
}

// new_ssl_conn returns a new SSLConn with the given config.
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}
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
	$if trace_ssl ? {
		eprintln(@METHOD)
	}
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
		net.shutdown(s.handle)
		net.close(s.handle)!
	}
}

// connect to server using mbedtls
fn (mut s SSLConn) init() ! {
	$if trace_ssl ? {
		eprintln(@METHOD)
	}
	C.mbedtls_net_init(&s.server_fd)
	C.mbedtls_ssl_init(&s.ssl)
	C.mbedtls_ssl_config_init(&s.conf)

	mut ret := 0
	ret = C.mbedtls_ssl_config_defaults(&s.conf, C.MBEDTLS_SSL_IS_CLIENT, C.MBEDTLS_SSL_TRANSPORT_STREAM,
		C.MBEDTLS_SSL_PRESET_DEFAULT)
	if ret != 0 {
		return error_with_code('Failed to set SSL configuration', ret)
	}

	unsafe {
		C.mbedtls_ssl_conf_rng(&s.conf, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)
	}

	if s.config.verify != '' || s.config.cert != '' || s.config.cert_key != '' {
		s.certs = &SSLCerts{}
		C.mbedtls_x509_crt_init(&s.certs.cacert)
		C.mbedtls_x509_crt_init(&s.certs.client_cert)
		C.mbedtls_pk_init(&s.certs.client_key)
	}

	if s.config.in_memory_verification {
		if s.config.verify != '' {
			ret = C.mbedtls_x509_crt_parse(&s.certs.cacert, s.config.verify.str,
				s.config.verify.len + 1)
		}
		if s.config.cert != '' {
			ret = C.mbedtls_x509_crt_parse(&s.certs.client_cert, s.config.cert.str,
				s.config.cert.len + 1)
		}
		if s.config.cert_key != '' {
			unsafe {
				ret = C.mbedtls_pk_parse_key(&s.certs.client_key, s.config.cert_key.str,
					s.config.cert_key.len + 1, 0, 0, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)
			}
		}
	} else {
		if s.config.verify != '' {
			ret = C.mbedtls_x509_crt_parse_file(&s.certs.cacert, &char(s.config.verify.str))
		}
		if s.config.cert != '' {
			ret = C.mbedtls_x509_crt_parse_file(&s.certs.client_cert, &char(s.config.cert.str))
		}
		if s.config.cert_key != '' {
			unsafe {
				ret = C.mbedtls_pk_parse_keyfile(&s.certs.client_key, &char(s.config.cert_key.str),
					0, C.mbedtls_ctr_drbg_random, &mbedtls.ctr_drbg)
			}
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
	$if trace_ssl ? {
		eprintln('${@METHOD} hostname: ${hostname}')
	}
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
	$if trace_ssl ? {
		eprintln('${@METHOD} hostname: ${hostname} | port: ${port}')
	}
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

// peer_addr retrieves the ip address and port number used by the peer
pub fn (s &SSLConn) peer_addr() !net.Addr {
	return net.peer_addr_from_socket_handle(s.handle)
}

// socket_read_into_ptr reads `len` bytes into `buf`
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
		res = C.mbedtls_ssl_read(&s.ssl, buf_ptr, len)
		if res > 0 {
			return res
		} else if res == 0 {
			$if trace_ssl ? {
				eprintln('${@METHOD} ---> res: io.Eof')
			}
			return io.Eof{}
		} else {
			match res {
				C.MBEDTLS_ERR_SSL_WANT_READ {
					s.wait_for_read(deadline - time.now()) or {
						$if trace_ssl ? {
							eprintln('${@METHOD} ---> res: ${err}, C.MBEDTLS_ERR_SSL_WANT_READ')
						}
						return err
					}
				}
				C.MBEDTLS_ERR_SSL_WANT_WRITE {
					s.wait_for_write(deadline - time.now()) or {
						$if trace_ssl ? {
							eprintln('${@METHOD} ---> res: ${err}, C.MBEDTLS_ERR_SSL_WANT_WRITE')
						}
						return err
					}
				}
				C.MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY {
					$if trace_ssl ? {
						eprintln('${@METHOD} ---> res: 0 C.MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY')
					}
					return 0
				}
				else {
					$if trace_ssl ? {
						eprintln('${@METHOD} ---> res: could not read using SSL')
					}
					return error_with_code('Could not read using SSL', res)
				}
			}
		}
	}

	// Dead code, for the compiler to pass
	return error('Unknown error')
}

// read reads data from the ssl connection into `buffer`
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
			mut sent := C.mbedtls_ssl_write(&s.ssl, ptr, remaining)
			if sent <= 0 {
				match sent {
					C.MBEDTLS_ERR_SSL_WANT_READ {
						s.wait_for_read(deadline - time.now())!
						continue
					}
					C.MBEDTLS_ERR_SSL_WANT_WRITE {
						s.wait_for_write(deadline - time.now())!
						continue
					}
					else {
						$if trace_ssl ? {
							eprintln('${@METHOD} ---> res: could not write SSL, sent: ${sent}')
						}
						return error_with_code('Could not write using SSL', sent)
					}
				}
			}
			total_sent += sent
		}
	}
	return total_sent
}

// write writes data from `bytes` to the ssl connection
pub fn (mut s SSLConn) write(bytes []u8) !int {
	return s.write_ptr(&u8(bytes.data), bytes.len)
}

// write_string writes a string to the ssl connection
pub fn (mut s SSLConn) write_string(str string) !int {
	$if trace_ssl ? {
		eprintln('${@METHOD} str: ${str}')
	}
	return s.write_ptr(str.str, str.len)
}

// Select waits for an io operation (specified by parameter `test`) to be available
fn @select(handle int, test Select, timeout time.Duration) !bool {
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
			tv_sec: u64(seconds)
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
				res = net.socket_error(C.@select(handle + 1, &set, C.NULL, C.NULL, timeval_timeout))!
			}
			.write {
				res = net.socket_error(C.@select(handle + 1, C.NULL, &set, C.NULL, timeval_timeout))!
			}
			.except {
				res = net.socket_error(C.@select(handle + 1, C.NULL, C.NULL, &set, timeval_timeout))!
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
	ready := @select(handle, what, timeout)!
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
