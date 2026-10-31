// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.urllib

// DirectorFn is a hook invoked to modify the outbound request before sending it to the upstream.
pub type DirectorFn = fn (mut req Request)

// ModifyResponseFn is a hook invoked to modify the response received from the upstream before returning it.
pub type ModifyResponseFn = fn (mut resp Response) !

// ErrorHandlerFn produces an HTTP response when communicating with the upstream fails.
pub type ErrorHandlerFn = fn (mut req Request, err IError) Response

// ReverseProxyConfig holds configuration options for ReverseProxy.
pub struct ReverseProxyConfig {
pub mut:
	// target is the upstream base URL to which requests will be proxied.
	target urllib.URL
	// director, if non-nil, modifies the outbound request before forwarding.
	director DirectorFn = unsafe { nil }
	// modify_response, if non-nil, inspects or alters the response returned from upstream.
	modify_response ModifyResponseFn = unsafe { nil }
	// error_handler produces an HTTP response when forwarding fails.
	// Defaults to default_reverse_proxy_error_handler (502 Bad Gateway).
	error_handler ErrorHandlerFn = unsafe { nil }
	// preserve_host controls whether the incoming Host header is forwarded unchanged (true),
	// or rewritten to target.host (false, default).
	preserve_host bool
}

// ReverseProxy is an HTTP Handler that takes an incoming request and
// sends it to another server, proxying the response back to the client.
pub struct ReverseProxy {
pub mut:
	config ReverseProxyConfig
}

// default_reverse_proxy_error_handler returns a 502 Bad Gateway response when upstream fails.
pub fn default_reverse_proxy_error_handler(mut req Request, err IError) Response {
	return Response{
		status_code: 502
		status_msg:  'Bad Gateway'
		header:      new_header(
			key:   .content_type
			value: 'text/plain; charset=utf-8'
		)
		body:        'Bad Gateway: ${err.msg()}\n'
	}
}

// new_single_host_reverse_proxy returns a new ReverseProxy that routes
// requests to the specified upstream `target_url`.
pub fn new_single_host_reverse_proxy(target_url string) !&ReverseProxy {
	target := urllib.parse(target_url)!
	if target.scheme !in ['http', 'https'] {
		return error('new_single_host_reverse_proxy: scheme must be http or https, got "${target.scheme}"')
	}
	if target.host == '' {
		return error('new_single_host_reverse_proxy: target host cannot be empty')
	}
	return &ReverseProxy{
		config: ReverseProxyConfig{
			target: target
		}
	}
}

// new_reverse_proxy returns a new ReverseProxy configured with `config`.
pub fn new_reverse_proxy(config ReverseProxyConfig) !&ReverseProxy {
	if config.target.scheme !in ['http', 'https'] {
		return error('new_reverse_proxy: scheme must be http or https, got "${config.target.scheme}"')
	}
	if config.target.host == '' {
		return error('new_reverse_proxy: target host cannot be empty')
	}
	return &ReverseProxy{
		config: config
	}
}

// handle satisfies the http.Handler interface so ReverseProxy can be used
// directly as an http.Server handler.
@[unsafe]
pub fn (mut rp ReverseProxy) handle(req Request) Response {
	mut out_req := req
	return rp.proxy(mut out_req) or {
		handler := if rp.config.error_handler != unsafe { nil } {
			rp.config.error_handler
		} else {
			default_reverse_proxy_error_handler
		}
		handler(mut out_req, err)
	}
}

// proxy forwards `req` to the upstream server and returns the response.
@[unsafe]
pub fn (rp &ReverseProxy) proxy(mut req Request) !Response {
	target := rp.config.target
	target_query := target.raw_query

	// Parse incoming request target/URL
	mut req_url := urllib.parse(req.url) or {
		return error('reverse_proxy: malformed request url "${req.url}"')
	}
	incoming_host := if req.host != '' { req.host } else { req.header.get(.host) or { '' } }
	incoming_proto := if req_url.scheme == 'https' { 'https' } else { 'http' }
	incoming_version := req.version

	// Join paths
	joined_path := join_url_path(target.path, req_url.path)
	joined_raw_path := join_url_path(target.escaped_path(), req_url.escaped_path())
	req_url.scheme = target.scheme
	req_url.host = target.host
	req_url.path = joined_path
	req_url.raw_path = joined_raw_path
	req_url.opaque = ''
	req_url.user = target.user
	req_url.fragment = ''

	// Merge query parameters
	if target_query != '' && req_url.raw_query != '' {
		req_url.raw_query = '${target_query}&${req_url.raw_query}'
	} else if target_query != '' {
		req_url.raw_query = target_query
	}

	req.url = req_url.str()

	// Strip client connection options before writing the upstream Host.
	clean_hop_by_hop_headers(mut req.header)
	// Update Host header
	if !rp.config.preserve_host {
		req.host = target.host
		req.header.set(.host, target.host)
	} else if incoming_host != '' {
		req.header.set(.host, incoming_host)
	}

	// Set standard proxy forwarding headers
	client_ip := req.remote_ip()
	if client_ip != '' {
		previous := req.header.values(.x_forwarded_for)
		if previous.len > 0 {
			prev_xff := previous.join(', ')
			req.header.set(.x_forwarded_for, '${prev_xff}, ${client_ip}')
		} else {
			req.header.set(.x_forwarded_for, client_ip)
		}
	}

	if !req.header.contains(.x_forwarded_host) && incoming_host != '' {
		req.header.set(.x_forwarded_host, incoming_host)
	}

	if !req.header.contains(.x_forwarded_proto) {
		req.header.set(.x_forwarded_proto, incoming_proto)
	}
	// Incoming HTTP/2 requests still use the client's default HTTP/1.1 transport.
	req.version = .v1_1
	req.validate = true
	req.raw_response_body = true

	// Run custom director hook if configured
	if rp.config.director != unsafe { nil } {
		rp.config.director(mut req)
	}
	// Request bodies are already decoded, and the director may have changed them.
	clean_hop_by_hop_headers(mut req.header)
	req.header.delete(.content_length)

	// Reverse proxies do not follow redirects on behalf of the client
	req.allow_redirect = false

	// Forward request to upstream
	mut resp := req.do()!

	// Strip hop-by-hop headers from upstream response
	clean_hop_by_hop_headers(mut resp.header)
	// The upstream protocol does not determine the downstream status line.
	resp.set_version(if incoming_version == .v1_0 { Version.v1_0 } else { Version.v1_1 })

	// Run custom response modifier if configured
	if rp.config.modify_response != unsafe { nil } {
		original_length := resp.body.len
		rp.config.modify_response(mut resp)!
		// Let the server frame the possibly modified body with its current length.
		if resp.body.len != original_length {
			resp.header.delete(.content_length)
		}
	}

	return resp
}

fn join_url_path(target_path string, req_path string) string {
	if target_path == '' || target_path == '/' {
		if req_path == '' || req_path == '/' {
			return '/'
		}
		if req_path.starts_with('/') {
			return req_path
		}
		return '/' + req_path
	}
	if req_path == '' {
		return target_path
	}
	t_has := target_path.ends_with('/')
	r_has := req_path.starts_with('/')
	if t_has && r_has {
		return target_path + req_path[1..]
	} else if !t_has && !r_has {
		return target_path + '/' + req_path
	}
	return target_path + req_path
}

const hop_by_hop_headers = [
	'connection',
	'proxy-connection',
	'keep-alive',
	'proxy-authenticate',
	'proxy-authorization',
	'te',
	'trailer',
	'trailers',
	'transfer-encoding',
	'upgrade',
]

fn clean_hop_by_hop_headers(mut h Header) {
	for conn in h.values(.connection) {
		for token in conn.split(',') {
			trimmed := token.trim_space()
			if trimmed != '' {
				h.delete_custom(trimmed)
			}
		}
	}
	for name in hop_by_hop_headers {
		h.delete_custom(name)
	}
}
