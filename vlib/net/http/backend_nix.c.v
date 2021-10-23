// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import strings
import net
import net.openssl
import os
import time

const (
	is_used = openssl.is_used
)

fn (mut req Request) ssl_do(port int, method Method, host_name string, path string) ?Response {
	// ssl_method := C.SSLv23_method()
	ctx := C.SSL_CTX_new(C.TLS_method())
	defer {
		if ctx != 0 {
			C.SSL_CTX_free(ctx)
		}
	}
	C.SSL_CTX_set_verify_depth(ctx, 4)
	flags := C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3 | C.SSL_OP_NO_COMPRESSION
	C.SSL_CTX_set_options(ctx, flags)
	// Support client certificates:
	mut verify := req.verify
	mut cert := req.cert
	mut cert_key := req.cert_key
	if req.in_memory_verification {
		now := time.now().unix.str()

		verify = os.temp_dir() + '/v_verify' + now
		cert = os.temp_dir() + '/v_cert' + now
		cert_key = os.temp_dir() + '/v_cert_key' + now

		if req.verify != '' {
			os.write_file(verify, req.verify) ?
		}
		if req.cert != '' {
			os.write_file(cert, req.cert) ?
		}
		if req.cert_key != '' {
			os.write_file(cert_key, req.cert_key) ?
		}
	}
	mut res := 0
	if req.verify != '' {
		res = C.SSL_CTX_load_verify_locations(ctx, &char(verify.str), 0)
		if req.validate && res != 1 {
			return error('http: openssl: SSL_CTX_load_verify_locations failed')
		}
	}
	if req.cert != '' {
		res = C.SSL_CTX_use_certificate_file(ctx, &char(cert.str), C.SSL_FILETYPE_PEM)
		if req.validate && res != 1 {
			return error('http: openssl: SSL_CTX_use_certificate_file failed, res: $res')
		}
	}
	if req.cert_key != '' {
		res = C.SSL_CTX_use_PrivateKey_file(ctx, &char(cert_key.str), C.SSL_FILETYPE_PEM)
		if req.validate && res != 1 {
			return error('http: openssl: SSL_CTX_use_PrivateKey_file failed, res: $res')
		}
	}

	if req.in_memory_verification {
		if req.verify != '' {
			os.rm(verify) ?
		}
		if req.cert != '' {
			os.rm(cert) ?
		}
		if req.cert_key != '' {
			os.rm(cert_key) ?
		}
	}

	// the setup is done, prepare an ssl connection from the SSL context:
	web := C.BIO_new_ssl_connect(ctx)
	defer {
		if web != 0 {
			C.BIO_free_all(web)
		}
	}
	addr := host_name + ':' + port.str()

	ssl := &openssl.SSL(0)
	C.BIO_get_ssl(web, &ssl)
	preferred_ciphers := 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4'
	res = C.SSL_set_cipher_list(voidptr(ssl), &char(preferred_ciphers.str))
	if res != 1 {
		return error('http: openssl: SSL_set_cipher_list failed, res: $res')
	}
	res = C.SSL_set_tlsext_host_name(voidptr(ssl), host_name.str)
	res = C.BIO_set_conn_hostname(web, addr.str)

	if req.use_proxy {
		req.proxy.prepare(req, '$host_name:$port') ?
		proxy_connection_fd := req.proxy.conn.sock.handle

		res = C.BIO_set_fd(web, proxy_connection_fd, C.BIO_NOCLOSE)

		if res != 0 {
			return error('http: openssl: BIO_set_fd failed, res: $res')
		}
	} else {
		res = C.BIO_do_connect(web)

		if res != 1 {
			return error('http: openssl: BIO_do_connect failed, res: $res')
		}
	}

	res = C.BIO_do_handshake(web)

	pcert := C.SSL_get_peer_certificate(voidptr(ssl))
	defer {
		if pcert != 0 {
			C.X509_free(pcert)
		}
	}
	res = C.SSL_get_verify_result(voidptr(ssl))
	if req.validate && res != C.X509_V_OK {
		return error('http: openssl: SSL_get_verify_result failed, res: $res')
	}
	// /////
	req_headers := req.build_request_headers(method, host_name, path)
	$if trace_http_request ? {
		eprintln('> $req_headers')
	}

	C.BIO_puts(web, &char(req_headers.str))

	mut content := strings.new_builder(100)
	mut buff := [bufsize]byte{}
	bp := unsafe { &buff[0] }
	mut readcounter := 0
	for {
		readcounter++
		len := unsafe { C.BIO_read(web, bp, bufsize) }
		if len <= 0 {
			break
		}

		$if debug_http ? {
			eprintln('ssl_do, read ${readcounter:4d} | len: $len')
			eprintln('-'.repeat(20))
			eprintln(unsafe { tos(bp, len) })
			eprintln('-'.repeat(20))
		}
		unsafe { content.write_ptr(bp, len) }
	}

	response_text := content.str()

	$if trace_http_response ? {
		eprintln('< $response_text')
	}
	return parse_response(response_text)
}

fn (mut proxy HttpProxy) create_ssl_tcp(hostname string, port int) ?&net.TcpConn {
	ctx := C.SSL_CTX_new(C.TLS_method())
	defer {
		if ctx != 0 {
			C.SSL_CTX_free(ctx)
		}
	}
	C.SSL_CTX_set_verify_depth(ctx, 4)
	flags := C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3 | C.SSL_OP_NO_COMPRESSION
	C.SSL_CTX_set_options(ctx, flags)

	// Support client certificates:
	mut verify := proxy.verify
	mut cert := proxy.cert
	mut cert_key := proxy.cert_key

	if proxy.in_memory_verification {
		now := time.now().unix.str()

		verify = os.temp_dir() + '/v_verify' + now
		cert = os.temp_dir() + '/v_cert' + now
		cert_key = os.temp_dir() + '/v_cert_key' + now

		if proxy.verify != '' {
			os.write_file(verify, proxy.verify) ?
		}
		if proxy.cert != '' {
			os.write_file(cert, proxy.cert) ?
		}
		if proxy.cert_key != '' {
			os.write_file(cert_key, proxy.cert_key) ?
		}
	}

	mut res := 0
	if proxy.verify != '' {
		res = C.SSL_CTX_load_verify_locations(ctx, &char(verify.str), 0)
		if proxy.validate && res != 1 {
			return error('http_proxy: openssl: SSL_CTX_load_verify_locations failed')
		}
	}
	if proxy.cert != '' {
		res = C.SSL_CTX_use_certificate_file(ctx, &char(cert.str), C.SSL_FILETYPE_PEM)
		if proxy.validate && res != 1 {
			return error('http_proxy: openssl: SSL_CTX_use_certificate_file failed, res: $res')
		}
	}
	if proxy.cert_key != '' {
		res = C.SSL_CTX_use_PrivateKey_file(ctx, &char(cert_key.str), C.SSL_FILETYPE_PEM)
		if proxy.validate && res != 1 {
			return error('http_proxy: openssl: SSL_CTX_use_PrivateKey_file failed, res: $res')
		}
	}

	if proxy.in_memory_verification {
		if proxy.verify != '' {
			os.rm(verify) ?
		}
		if proxy.cert != '' {
			os.rm(cert) ?
		}
		if proxy.cert_key != '' {
			os.rm(cert_key) ?
		}
	}

	// the setup is done, prepare an ssl connection from the SSL context:
	web := C.BIO_new_ssl_connect(ctx)
	defer {
		if web != 0 {
			C.BIO_free_all(web)
		}
	}

	ssl := &openssl.SSL(0)
	C.BIO_get_ssl(web, &ssl)

	preferred_ciphers := 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4'
	res = C.SSL_set_cipher_list(voidptr(ssl), &char(preferred_ciphers.str))

	if res != 1 {
		return error('http_proxy: openssl: SSL_set_cipher_list failed, res: $res')
	}

	addr := '$hostname:$port'

	res = C.SSL_set_tlsext_host_name(voidptr(ssl), hostname.str)
	res = C.BIO_set_conn_hostname(web, addr.str)

	res = C.BIO_do_connect(web)

	if res != 1 {
		return error('http_proxy: openssl: BIO_do_connect failed, res: $res')
	}

	res = C.BIO_do_handshake(web)

	pcert := C.SSL_get_peer_certificate(voidptr(ssl))
	defer {
		if pcert != 0 {
			C.X509_free(pcert)
		}
	}

	res = C.SSL_get_verify_result(voidptr(ssl))
	if proxy.validate && res != C.X509_V_OK {
		return error('http_proxy: openssl: SSL_get_verify_result failed, res: $res')
	}

	connection_fd := C.BIO_get_fd(web, 0)

	if !(connection_fd > 0) {
		return error('http_proxy: openssl: could not get fd')
	}

	return net.tcp_conn_from_handle(connection_fd)
}
