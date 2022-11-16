// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.ssl
import strings

fn (req &Request) ssl_do(port int, method Method, host_name string, path string) !Response {
	mut ssl_conn := ssl.new_ssl_conn(
		verify: req.verify
		cert: req.cert
		cert_key: req.cert_key
		validate: req.validate
		in_memory_verification: req.in_memory_verification
	)!
	ssl_conn.dial(host_name, port) or { return err }

	req_headers := req.build_request_headers(method, host_name, path)
	$if trace_http_request ? {
		eprintln('> ${req_headers}')
	}
	// println(req_headers)
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
		unsafe { content.write_ptr(bp, len) }
	}
	ssl_conn.shutdown()!
	response_text := content.str()
	$if trace_http_response ? {
		eprintln('< ${response_text}')
	}
	return parse_response(response_text)
}
