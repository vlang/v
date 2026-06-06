// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.ssl
import strings

fn (req &Request) ssl_do(port int, method Method, host_name string, path string, data string, header Header) !Response {
	$if windows && !no_vschannel ? {
		return vschannel_ssl_do(req, port, method, host_name, path, data, header)
	}
	return net_ssl_do(req, port, method, host_name, path, data, header)
}

fn net_ssl_do(req &Request, port int, method Method, host_name string, path string, data string, header Header) !Response {
	mut retries := 0
	req_headers := req.build_request_headers_with(method, host_name, port, path, data, header)
	$if trace_http_request ? {
		eprint('> ')
		eprint(req_headers)
		eprintln('')
	}
	// Advertise ALPN `h2` (with an `http/1.1` fallback) only when HTTP/2 is
	// requested, so existing callers see no change on the wire. Requests that
	// rely on streaming response callbacks or stop limits stay on HTTP/1.1,
	// since the HTTP/2 path buffers the full response; the ALPN offer is gated
	// here (before negotiation) because once a server selects `h2` we cannot
	// fall back to HTTP/1.1 framing on the same connection.
	use_h2 := req.enable_http2 && !req.uses_response_streaming()
	alpn := if use_h2 { ['h2', 'http/1.1'] } else { []string{} }
	for {
		mut ssl_conn := ssl.new_ssl_conn(
			verify:                 req.verify
			cert:                   req.cert
			cert_key:               req.cert_key
			validate:               req.validate
			in_memory_verification: req.in_memory_verification
			alpn_protocols:         alpn
		)!
		ssl_conn.dial(host_name, port) or {
			retries++
			if is_no_need_retry_error(err.code()) || retries >= req.max_retries {
				return err
			}
			continue
		}
		// Propagate the request's read timeout into the SSL backend.
		// Without this, mbedtls keeps its init-time default and openssl falls back to no
		// timeout at all on a stalled socket — see issue surfaced by macOS arm64 + tcc CI hangs.
		if req.read_timeout > 0 {
			ssl_conn.set_read_timeout(req.read_timeout)
		}
		// If the server negotiated HTTP/2 via ALPN, speak it; otherwise fall
		// back to the existing HTTP/1.1 path unchanged.
		if use_h2 && ssl_conn.negotiated_alpn() == 'h2' {
			return req.h2_do(mut ssl_conn, method, host_name, port, path, data, header)!
		}
		return req.do_request(req_headers, mut ssl_conn)!
	}
	return error('http.net_ssl_do: exhausted retries')
}

// h2_do runs a single request over an HTTP/2 connection on an already-dialled,
// ALPN-negotiated `h2` TLS socket, and returns the response as a net.http
// Response.
fn (req &Request) h2_do(mut ssl_conn ssl.SSLConn, method Method, host_name string, port int, path string, data string, header Header) !Response {
	defer {
		ssl_conn.shutdown() or {}
	}
	h2req := req.to_h2_request(method, h2_authority(host_name, port), path, data, header)
	mut conn := new_h2_conn(ssl_conn)
	h2resp := conn.do(h2req)!
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(h2resp.body.len))!
	}
	return h2_response_to_http(h2resp)
}

fn read_from_ssl_connection_cb(con voidptr, buf &u8, bufsize int) !int {
	mut ssl_conn := unsafe { &ssl.SSLConn(con) }
	return ssl_conn.socket_read_into_ptr(buf, bufsize)
}

fn (req &Request) do_request(req_headers string, mut ssl_conn ssl.SSLConn) !Response {
	defer {
		ssl_conn.shutdown() or {}
	}
	ssl_conn.write_string(req_headers) or { return err }
	mut content := strings.new_builder(4096)
	response_info := req.receive_all_data_from_cb_in_builder(mut content, voidptr(ssl_conn),
		read_from_ssl_connection_cb)!
	response_text := content.str()
	$if trace_http_response ? {
		eprint('< ')
		eprint(response_text)
		eprintln('')
	}
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(response_text.len))!
	}
	return parse_received_response(response_text, response_info)
}
