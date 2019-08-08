// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

// import strings 

// #flag windows -I @VROOT/thirdparty/openssl/include 
// #flag darwin -I @VROOT/thirdparty/openssl/include 
// #flag darwin linux -lssl
// // MacPorts
// #flag darwin -L/opt/local/lib
 
// #include <openssl/ssl.h>

// struct C.SSL {
 
// } 

fn init_module() {
	$if mac { 
		C.SSL_library_init() 
	} 
	$if linux { 
		C.SSL_library_init() 
	} 
	//C.SSL_load_error_strings() 
	//C.OPENSSL_config(0) 
}

fn ssl_do(method, host_name, path string) string { 
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
	addr := host_name + ':443' 
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
	out := C.BIO_new_fp(stdout, C.BIO_NOCLOSE) 
	res = C.BIO_do_connect(web) 
	res = C.BIO_do_handshake(web) 
	cert := C.SSL_get_peer_certificate(ssl) 
	res = C.SSL_get_verify_result(ssl) 
	/////// 
	s := '$method $path HTTP/1.1\r\n' + 
	     'Host: $host_name\r\n' + 
	     'Connection: close\r\n\r\n' 
	C.BIO_puts(web, s.str) 
	C.BIO_puts(out, '\n') 
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
	if !isnil(out) { 
		C.BIO_free(out) 
	} 
	if !isnil(web) { 
		C.BIO_free_all(web)
	} 
	if !isnil(ctx) { 
		C.SSL_CTX_free(ctx) 
	} 
	return sb.str() 
}
