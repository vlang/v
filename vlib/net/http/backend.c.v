// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.ssl
import strings

fn (req &Request) ssl_do(port int, method Method, host_name string, path string) !Response {
	$if windows && !no_vschannel ? {
		return vschannel_ssl_do(req, port, method, host_name, path)
	}
	return net_ssl_do(req, port, method, host_name, path)
}

fn net_ssl_do(req &Request, port int, method Method, host_name string, path string) !Response {
	mut ssl_conn := ssl.new_ssl_conn(
		verify: req.verify
		cert: req.cert
		cert_key: req.cert_key
		validate: req.validate
		in_memory_verification: req.in_memory_verification
	)!
	mut retries := 0
	for {
		ssl_conn.dial(host_name, port) or {
			retries++
			if is_no_need_retry_error(err.code()) || retries >= req.max_retries {
				return err
			}
			continue
		}
		break
	}

	req_headers := req.build_request_headers(method, host_name, path)
	$if trace_http_request ? {
		eprintln('> ${req_headers}')
	}

	return req.do_request(req_headers, mut ssl_conn)!
}

fn (req &Request) do_request(req_headers string, mut ssl_conn ssl.SSLConn) !Response {
	ssl_conn.write_string(req_headers) or { return err }

	mut content := strings.new_builder(100)
	mut buff := [bufsize]u8{}
	bp := unsafe { &buff[0] }
	mut readcounter := 0
	for {
		readcounter++
		len := ssl_conn.socket_read_into_ptr(bp, bufsize) or { break }
		$if debug_http ? {
			eprintln('ssl_do, read ${readcounter:4d} | len: ${len}')
			eprintln('-'.repeat(20))
			eprintln(unsafe { tos(bp, len) })
			eprintln('-'.repeat(20))
		}
		if len <= 0 {
			break
		}
		unsafe { content.write_ptr(bp, len) }
		if req.on_progress != unsafe { nil } {
			req.on_progress(req, content[content.len - len..], u64(content.len))!
		}
	}
	ssl_conn.shutdown()!
	response_text := content.str()
	$if trace_http_response ? {
		eprintln('< ${response_text}')
	}
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(response_text.len))!
	}
	return parse_response(response_text)
}
