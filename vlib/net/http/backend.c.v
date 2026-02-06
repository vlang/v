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
		verify:                 req.verify
		cert:                   req.cert
		cert_key:               req.cert_key
		validate:               req.validate
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

	req_headers := req.build_request_headers(method, host_name, port, path)
	$if trace_http_request ? {
		eprint('> ')
		eprint(req_headers)
		eprintln('')
	}

	return req.do_request(req_headers, mut ssl_conn)!
}

fn read_from_ssl_connection_cb(con voidptr, buf &u8, bufsize int) !int {
	mut ssl_conn := unsafe { &ssl.SSLConn(con) }
	return ssl_conn.socket_read_into_ptr(buf, bufsize)
}

fn (req &Request) do_request(req_headers string, mut ssl_conn ssl.SSLConn) !Response {
	ssl_conn.write_string(req_headers) or { return err }
	mut content := strings.new_builder(4096)
	req.receive_all_data_from_cb_in_builder(mut content, voidptr(ssl_conn), read_from_ssl_connection_cb)!
	ssl_conn.shutdown()!
	response_text := content.str()
	$if trace_http_response ? {
		eprint('< ')
		eprint(response_text)
		eprintln('')
	}
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(response_text.len))!
	}
	return parse_response(response_text)
}
