// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
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

struct C.SSL {
 
} 

fn init() int {
	C.SSL_library_init() 
	return 1
}

fn (req &Request) ssl_do(port int, method, host_name, path string) Response {
	//ssl_method := C.SSLv23_method() 
	ssl_method := C.TLSv1_2_method() 
	if isnil(method) { 
	} 
	ctx := C.SSL_CTX_new(ssl_method) 
	if isnil(ctx) { 
	} 
	C.SSL_CTX_set_verify_depth(ctx, 4) 
	flags := C.SSL_OP_NO_SSLv2 | C.SSL_OP_NO_SSLv3 | C.SSL_OP_NO_COMPRESSION 
	C.SSL_CTX_set_options(ctx, flags) 
	mut res := C.SSL_CTX_load_verify_locations(ctx, 'random-org-chain.pem', 0) 
	if res != 1 {
	} 
	web := C.BIO_new_ssl_connect(ctx) 
	if isnil(ctx) { 
	} 
	addr := host_name + ':' + port.str()
	res = C.BIO_set_conn_hostname(web, addr.str) 
	if res != 1 {
	} 
	ssl := &C.SSL{!} 
	C.BIO_get_ssl(web, &ssl) 
	if isnil(ssl) { 
	} 
	preferred_ciphers := 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4' 
	res = C.SSL_set_cipher_list(ssl, preferred_ciphers.str) 
	if res != 1 {
	} 
	res = C.SSL_set_tlsext_host_name(ssl, host_name.str) 
	res = C.BIO_do_connect(web) 
	res = C.BIO_do_handshake(web) 
	cert := C.SSL_get_peer_certificate(ssl) 
	res = C.SSL_get_verify_result(ssl) 
	///////
	s := req.build_request_headers(method, host_name, path)
	C.BIO_puts(web, s.str) 
	mut sb := strings.new_builder(100) 
	for {
		buff := [1536]byte 
		len := int(C.BIO_read(web, buff, 1536) ) 
		if len > 0 { 
			sb.write(tos(buff, len)) 
		} 
		else {
			break 
		} 
	} 
	if !isnil(web) { 
		C.BIO_free_all(web)
	} 
	if !isnil(ctx) { 
		C.SSL_CTX_free(ctx) 
	}

	return parse_response(sb.str())
}
