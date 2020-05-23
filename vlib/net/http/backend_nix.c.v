// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import strings

// On linux, prefer a localy build openssl, because it is
// much more likely for it to be newer, than the system
// openssl from libssl-dev. If there is no local openssl,
// the next flag is harmless, since it will still use the
// (older) system openssl.
#flag linux -I/usr/local/include/openssl -L/usr/local/lib
#flag -l ssl -l crypto
// MacPorts
#flag darwin -I/opt/local/include
#flag darwin -L/opt/local/lib
// Brew
#flag darwin -I/usr/local/opt/openssl/include
#flag darwin -L/usr/local/opt/openssl/lib
#include <openssl/ssl.h>

struct C.ssl_st {}

fn C.SSL_library_init()

fn C.TLSv1_2_method() voidptr

fn C.SSL_CTX_set_options()

fn C.SSL_CTX_new() voidptr

fn C.SSL_CTX_set_verify_depth()

fn C.SSL_CTX_load_verify_locations() int

fn C.BIO_new_ssl_connect() voidptr

fn C.BIO_set_conn_hostname() int

fn C.BIO_get_ssl()

fn C.SSL_set_cipher_list() int

fn C.BIO_do_connect() int

fn C.BIO_do_handshake() int

fn C.SSL_get_peer_certificate() int

fn C.SSL_get_verify_result() int

fn C.SSL_set_tlsext_host_name() int

fn C.BIO_puts()

fn C.BIO_read() int

fn C.BIO_free_all()

fn C.SSL_CTX_free()

fn init() int {
	C.SSL_library_init()
	return 1
}

fn (req &Request) ssl_do(port int, method, host_name, path string) ?Response {
	// ssl_method := C.SSLv23_method()
	ssl_method := C.TLSv1_2_method()
	ctx := C.SSL_CTX_new(ssl_method)
	C.SSL_CTX_set_verify_depth(ctx, 4)
	flags := C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3 | C.SSL_OP_NO_COMPRESSION
	C.SSL_CTX_set_options(ctx, flags)
	mut res := C.SSL_CTX_load_verify_locations(ctx, 'random-org-chain.pem', 0)
	web := C.BIO_new_ssl_connect(ctx)
	addr := host_name + ':' + port.str()
	res = C.BIO_set_conn_hostname(web, addr.str)
	ssl := &C.ssl_st(0)
	C.BIO_get_ssl(web, &ssl)
	preferred_ciphers := 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4'
	res = C.SSL_set_cipher_list(ssl, preferred_ciphers.str)
	if res != 1 {
		println('http: openssl: cipher failed')
	}
	res = C.SSL_set_tlsext_host_name(ssl, host_name.str)
	res = C.BIO_do_connect(web)
	if res != 1 {
		return error('cannot connect the endpoint')
	}
	res = C.BIO_do_handshake(web)
	C.SSL_get_peer_certificate(ssl)
	res = C.SSL_get_verify_result(ssl)
	// /////
	req_headers := req.build_request_headers(method, host_name, path)
	C.BIO_puts(web, req_headers.str)
	mut content := strings.new_builder(100)
	mut buff := [bufsize]byte
	mut readcounter := 0
	for {
		readcounter++
		len := C.BIO_read(web, buff, bufsize)
		if len <= 0 {
			break
		}
		$if debug_http ? {
			eprintln('ssl_do, read ${readcounter:4d} | len: $len')
			eprintln('-'.repeat(20))
			eprintln(tos(buff, len))
			eprintln('-'.repeat(20))
		}
		content.write_bytes(buff, len)
	}
	if web != 0 {
		C.BIO_free_all(web)
	}
	if ctx != 0 {
		C.SSL_CTX_free(ctx)
	}
	return parse_response(content.str())
}
