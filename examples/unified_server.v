// Unified HTTP Server Example
// Demonstrates a single handler serving HTTP/1.1, HTTP/2, and HTTP/3.
module main

import net.http

struct AppHandler {}

fn (h AppHandler) handle(req http.ServerRequest) http.ServerResponse {
	body := match req.path {
		'/' { 'Welcome! Protocol: ${req.version}' }
		'/api' { '{"status":"ok","protocol":"${req.version}"}' }
		else { 'Not found' }
	}
	status := if req.path == '/' || req.path == '/api' { 200 } else { 404 }
	return http.ServerResponse{
		status_code: status
		body: body.bytes()
	}
}

fn main() {
	mut server := http.Server{
		addr: '0.0.0.0:8080'
		handler: AppHandler{}
	}
	// For HTTP/1.1 only:
	server.listen_and_serve()
	// For HTTPS with automatic HTTP/2 + HTTP/3:
	// server.cert_file = 'cert.pem'
	// server.key_file = 'key.pem'
	// server.tls_addr = ':8443'
	// server.h3_addr = ':8443'
	// server.enable_h3 = true
	// server.listen_and_serve_all()!
}
