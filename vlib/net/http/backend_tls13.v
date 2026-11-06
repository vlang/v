// Copyright (c) 2026 V language contributors. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import os
import strings
import time
import x.net.tls as tls13
import x.net.tls.pki

// tls13_do runs one HTTP/1.x exchange over x.net.tls.Conn and closes the
// connection after the response.
fn (req &Request) tls13_do(port int, method Method, host_name string, path string, data string, header Header) !Response {
	check_header_value('User-Agent', req.user_agent)!
	raw := req.build_request_headers_opts(method, host_name, port, 443, path, data, header,
		true)!
	mut conn := req.dial_tls13(port, host_name)!
	defer {
		conn.close() or {}
	}
	$if trace_http_request ? {
		eprint('> ')
		eprint(raw)
		eprintln('')
	}
	resp, _ := req.h1_exchange_tls13(mut conn, raw) or {
		if err is ProgressCallbackError {
			return err
		}
		if !transport_is_idempotent(method) {
			return error_with_code(err.msg(), transport_err_unsafe_retry)
		}
		return err
	}
	return resp
}

fn (req &Request) dial_tls13(port int, host_name string) !&tls13.Conn {
	if req.enable_http2 {
		return error('net.http TLS 1.3 adapter: HTTP/2 is not supported')
	}
	if req.enable_http3 {
		return error('net.http TLS 1.3 adapter: HTTP/3 is not supported')
	}
	if req.proxy != unsafe { nil } {
		return error('net.http TLS 1.3 adapter: proxies are not supported')
	}
	if req.cert != '' || req.cert_key != '' {
		return error('net.http TLS 1.3 adapter: client certificates are not supported')
	}
	config := tls13_config(req, host_name)!
	address := if host_name.contains(':') {
		'[${host_name}]:${port}'
	} else {
		'${host_name}:${port}'
	}
	return tls13.dial(address, config)
}

fn (req &Request) h1_exchange_tls13(mut conn tls13.Conn, raw string) !(Response, bool) {
	tls13_write_all(mut conn, raw.bytes())!

	mut content := strings.new_builder(4096)
	response_info := req.receive_all_data_from_cb_in_builder(mut content, voidptr(conn),
		read_from_tls13_connection_cb)!
	response_text := content.str()
	$if trace_http_response ? {
		eprint('< ')
		eprint(response_text)
		eprintln('')
	}
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(response_text.len))!
	}
	resp := parse_received_response(response_text, response_info)!
	return resp, response_info.reusable
}

fn tls13_config(req &Request, host_name string) !tls13.Config {
	base := tls13.Config{
		server_name:          host_name
		insecure_skip_verify: !req.validate
		timeout:              tls13_timeout(req.read_timeout, req.write_timeout)
	}
	if req.verify == '' {
		return base
	}
	ca_pem := if req.in_memory_verification {
		req.verify
	} else {
		os.read_file(req.verify)!
	}
	mut roots := pki.new_trust_store()
	if roots.add_pem(ca_pem)! == 0 {
		return error('net.http TLS 1.3 adapter: no certificates found in verify bundle')
	}
	return tls13.Config{
		server_name:          host_name
		insecure_skip_verify: !req.validate
		root_cas:             roots
		timeout:              tls13_timeout(req.read_timeout, req.write_timeout)
	}
}

fn tls13_timeout(read_timeout i64, write_timeout i64) time.Duration {
	if read_timeout > 0 && write_timeout > 0 {
		return time.Duration(if read_timeout < write_timeout {
			read_timeout
		} else {
			write_timeout
		})
	}
	if read_timeout > 0 {
		return time.Duration(read_timeout)
	}
	if write_timeout > 0 {
		return time.Duration(write_timeout)
	}
	return 30 * time.second
}

fn tls13_write_all(mut conn tls13.Conn, data []u8) ! {
	mut offset := 0
	for offset < data.len {
		n := conn.write(data[offset..])!
		if n <= 0 {
			return error('net.http TLS 1.3 adapter: connection write made no progress')
		}
		offset += n
	}
}

fn read_from_tls13_connection_cb(con voidptr, buf &u8, bufsize int) !int {
	mut conn := unsafe { &tls13.Conn(con) }
	mut bytes := unsafe { buf.vbytes(bufsize) }
	return conn.read(mut bytes)
}
