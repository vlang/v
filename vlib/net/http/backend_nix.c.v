// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import strings
import net.openssl

const (
	is_used = openssl.is_used
)

fn (req &Request) ssl_do(port int, method Method, host_name string, path string) ?Response {
	// ssl_method := C.SSLv23_method()
	ctx := C.SSL_CTX_new(C.TLSv1_2_method())
	C.SSL_CTX_set_verify_depth(ctx, 4)
	flags := C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3 | C.SSL_OP_NO_COMPRESSION
	C.SSL_CTX_set_options(ctx, flags)
	mut res := C.SSL_CTX_load_verify_locations(ctx, 'random-org-chain.pem', 0)
	web := C.BIO_new_ssl_connect(ctx)
	addr := host_name + ':' + port.str()
	res = C.BIO_set_conn_hostname(web, addr.str)
	ssl := &openssl.SSL(0)
	C.BIO_get_ssl(web, &ssl)
	preferred_ciphers := 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4'
	res = C.SSL_set_cipher_list(voidptr(ssl), preferred_ciphers.str)
	if res != 1 {
		println('http: openssl: cipher failed')
	}
	res = C.SSL_set_tlsext_host_name(voidptr(ssl), host_name.str)
	res = C.BIO_do_connect(web)
	if res != 1 {
		return error('cannot connect the endpoint')
	}
	res = C.BIO_do_handshake(web)
	C.SSL_get_peer_certificate(voidptr(ssl))
	res = C.SSL_get_verify_result(voidptr(ssl))
	// /////
	req_headers := req.build_request_headers(method, host_name, path)
	// println(req_headers)
	C.BIO_puts(web, req_headers.str)
	mut content := strings.new_builder(100)
	mut buff := [bufsize]byte{}
	bp := &buff[0]
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
		unsafe { content.write_bytes(bp, len) }
	}
	if web != 0 {
		C.BIO_free_all(web)
	}
	if ctx != 0 {
		C.SSL_CTX_free(ctx)
	}
	return parse_response(content.str())
}
