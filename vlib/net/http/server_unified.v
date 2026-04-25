// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// Unified server entry points for multi-protocol serving.
// v3 (QUIC) support lives in server_unified_d_use_ngtcp2.v and is only
// compiled when `-d use_ngtcp2` is passed.
import net.http.common
import net.http.v2

// listen_and_serve_tls starts the shared HTTPS entry point for the unified server.
// Serves HTTP/2 over TLS and can also start HTTP/3 over UDP when compiled
// with `-d use_ngtcp2`.
//
// Note: The v2 TLS listener advertises only ALPN `h2`, so pure HTTPS
// HTTP/1.1 clients cannot connect on this port. Use listen_and_serve_all()
// to also serve HTTP/1.1 on a plain TCP port.
pub fn (mut s Server) listen_and_serve_tls() ! {
	if s.cert_file == '' || s.key_file == '' {
		return error('cert_file and key_file are required for TLS')
	}
	tls_addr := if s.tls_addr != '' { s.tls_addr } else { s.addr }

	handler_fn := fn [s] (req common.ServerRequest) common.ServerResponse {
		mut h := s.handler
		return h.handle(req)
	}

	maybe_start_h3(s, tls_addr, handler_fn)

	v2_config := v2.ServerConfig{
		addr:      tls_addr
		cert_file: s.cert_file
		key_file:  s.key_file
	}
	mut v2_server := v2.new_server(v2_config, handler_fn)!
	v2_server.listen_and_serve()!
}

// listen_and_serve_all starts listeners for all configured protocols:
// - HTTP/1.1 on plain TCP (always, blocking)
// - HTTP/2 over TLS when cert_file and key_file are set
// - HTTP/3 over UDP when enable_h3 is true and TLS is configured
//
// The same handler processes requests from all protocols.
// Configure `addr` for HTTP/1.1 and `tls_addr`/`h3_addr` for encrypted traffic.
pub fn (mut s Server) listen_and_serve_all() ! {
	if s.cert_file != '' && s.key_file != '' && s.tls_addr == '' {
		return error('listen_and_serve_all: tls_addr is required when TLS is enabled')
	}
	handler_fn := fn [s] (req common.ServerRequest) common.ServerResponse {
		mut h := s.handler
		return h.handle(req)
	}

	if s.cert_file != '' && s.key_file != '' {
		tls_addr := s.tls_addr

		maybe_start_h3(s, tls_addr, handler_fn)

		spawn start_h2_server(v2.ServerConfig{
			addr:      tls_addr
			cert_file: s.cert_file
			key_file:  s.key_file
		}, handler_fn)
	}

	s.listen_and_serve()
}

fn start_h2_server(config v2.ServerConfig, handler v2.Handler) {
	mut server := v2.new_server(config, handler) or {
		eprintln('[HTTP/2] failed to start: ${err}')
		return
	}
	server.listen_and_serve() or {
		eprintln('[HTTP/2] server error: ${err}')
	}
}
