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

/*
* SslConnLayer
*/
struct SslConnLayer {
	bio &C.BIO
}

fn (mut l SslConnLayer) write(buf []byte) ?int {
	byte_ptr := unsafe { &buf[0] }
	buf_len := buf.len
	return C.BIO_write(l.bio, byte_ptr, buf_len)
}

fn (mut l SslConnLayer) read(mut bytes []byte) ?int {
	bp := unsafe { &bytes[0] }
	ret := C.BIO_read(l.bio, bp, bytes.len)

	if ret <= 0 {
		// this assumes the OpenSSL default is blocking
		return error('error reading from SSL layer/EOF')
	}

	return ret
}

fn (mut l SslConnLayer) close() ? {
	C.BIO_free_all(l.bio)
}

// noops
fn (mut l SslConnLayer) set_read_timeout(t time.Duration) {}

fn (mut l SslConnLayer) set_write_timeout(t time.Duration) {}

/*
* ProxyConnLayer specific methods
 *
 * can be used to chain ProxyConnLayer and BIOs
*/
fn new_biom_from_layer(mut l ProxyConnLayer) &C.BIO_METHOD {
	bio_method_idx := C.BIO_get_new_index()
	bio_method_name := 'ProxyConnLayer$bio_method_idx'
	bio_method := C.BIO_meth_new(bio_method_idx, bio_method_name.str)

	$if debug_http_proxy ? {
		eprintln('proxy_conn_layer: creating OpenSSL BIO: $bio_method_name')
		eprintln('proxy_conn_layer: layer addr: ${voidptr(l)}')
	}

	bio_write_fn := fn [mut l] (b &C.BIO, data &byte, buflen int) int {
		mut data_buf := unsafe { data.vbytes(buflen) }

		return l.write(data_buf) or { -1 }
	}

	bio_read_fn := fn [mut l] (_ &C.BIO, data &byte, buflen int) int {
		mut data_buf := unsafe { data.vbytes(buflen) }

		return l.read(mut data_buf) or { -1 }
	}

	bio_puts_fn := fn [mut l] (_ &C.BIO, data &byte) int {
		data_str := unsafe { cstring_to_vstring(data) }
		data_array := data_str.bytes()

		return l.write(data_array) or { -1 }
	}

	bio_gets_fn := fn (bio &C.BIO, data &byte, buflen int) int {
		return -1
	}

	bio_create_fn := fn (b &C.BIO) int {
		$if debug_http_proxy ? {
			eprintln('proxy_conn_layer: instantiating BIO')
		}

		C.BIO_set_shutdown(b, 1)
		C.BIO_set_init(b, 1)
		return 1
	}

	bio_destroy_fn := fn [bio_method] (b &C.BIO) int {
		$if debug_http_proxy ? {
			eprintln('proxy_conn_layer: destroying BIO')
		}

		// l.close() or {}
		C.BIO_set_init(b, 0)
		C.BIO_meth_free(bio_method)
		return 1
	}

	// noops
	bio_ctrl_fn := fn (b &C.BIO, cb int, arg0 i64, arg1 voidptr) i64 {
		$if debug_http_proxy ? {
			eprintln('proxy_conn_layer: BIO ctrl: $cb')
		}

		match cb {
			C.BIO_CTRL_POP { return 0 }
			C.BIO_CTRL_PUSH { return 0 }
			C.BIO_CTRL_FLUSH { return 1 }
			else { return 1 }
		}
	}

	mut ret := 0
	ret += C.BIO_meth_set_create(bio_method, bio_create_fn)
	ret += C.BIO_meth_set_destroy(bio_method, bio_destroy_fn)
	ret += C.BIO_meth_set_read(bio_method, bio_read_fn)
	ret += C.BIO_meth_set_write(bio_method, bio_write_fn)
	ret += C.BIO_meth_set_gets(bio_method, bio_gets_fn)
	ret += C.BIO_meth_set_puts(bio_method, bio_puts_fn)
	ret += C.BIO_meth_set_ctrl(bio_method, bio_ctrl_fn)
	ret += C.BIO_meth_set_callback_ctrl(bio_method, C.NULL)

	if ret != 8 {
		return C.NULL
	}

	return bio_method
}

/*
* HTTP Proxy specific methods
*/
fn (mut proxy HttpProxy) create_ssl_layer(hostname string, port int) ?ProxyConnLayer {
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
		err_msg := unsafe {
			cstring_to_vstring(C.ERR_error_string(C.ERR_get_error(), 0))
		}
		return error('http_proxy: openssl: BIO_do_connect failed, msg: $err_msg')
	}

	res = C.BIO_do_handshake(web)

	if res != 1 {
		err_msg := unsafe {
			cstring_to_vstring(C.ERR_error_string(C.ERR_get_error(), 0))
		}
		return error('http_proxy: openssl: BIO_do_handshake failed, msg: $err_msg')
	}

	pcert := C.SSL_get_peer_certificate(voidptr(ssl))
	defer {
		if pcert != 0 {
			C.X509_free(pcert)
		}
	}

	res = C.SSL_get_verify_result(voidptr(ssl))
	// if proxy.validate && res != C.X509_V_OK {
	// return error('http_proxy: openssl: SSL_get_verify_result failed, res: $res')
	//}

	return SslConnLayer{
		bio: web
	}
}

/*
* Request specific methods
*/
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
			os.write_file(cert, req.verify) ?
		}
		if req.cert_key != '' {
			os.rm(cert_key) ?
		}
	}

	// the setup is done, prepare an ssl connection from the SSL context:
	mut web := if req.use_proxy { C.BIO_new_ssl(ctx, 1) } else { C.BIO_new_ssl_connect(ctx) }

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
		return error('http: openssl: SSL_set_cipher_list failed, res: $res')
	}
	res = C.SSL_set_tlsext_host_name(voidptr(ssl), host_name.str)

	if req.use_proxy {
		req.proxy.prepare(req, '$host_name:$port') ?
		mut c := &req.proxy.conn

		used_bio_method := new_biom_from_layer(mut c)
		interim_bio := C.BIO_new(used_bio_method)

		if used_bio_method == 0 || interim_bio == 0 {
			err_msg := unsafe {
				cstring_to_vstring(C.ERR_error_string(C.ERR_get_error(), 0))
			}
			return error(err_msg)
		}

		C.BIO_push(web, interim_bio)
		res = C.BIO_do_handshake(web)
	} else {
		addr := host_name + ':' + port.str()
		res = C.BIO_set_conn_hostname(web, addr.str)
		res = C.BIO_do_connect(web)

		if res != 1 {
			return error('http: openssl: BIO_do_connect failed, res: $res')
		}
	}

	res = C.BIO_do_handshake(web)
	if res <= 0 {
		err_msg := unsafe {
			cstring_to_vstring(C.ERR_error_string(C.ERR_get_error(), 0))
		}
		return error(err_msg)
	}

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

fn (mut proxy HttpProxy) create_ssl_tcp(hostname string, port int) ?ProxyConnLayer {
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

	return SslConnLayer{
		bio: web
	}
}
