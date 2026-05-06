// HTTP/2 Server Example
// Demonstrates HTTP/2 server using the unified net.http API.
// HTTP/2 is enabled automatically over TLS with ALPN h2 negotiation.
//
// To generate test certificates:
//   openssl req -x509 -newkey rsa:2048 -nodes \
//     -keyout key.pem -out cert.pem -days 365 \
//     -subj "/CN=localhost"
//
// Test with: curl -k --http2 https://localhost:8080/
module main

import net.http

struct AppHandler {}

fn (h AppHandler) handle(req http.ServerRequest) http.ServerResponse {
	println('Received: ${req.method} ${req.path} (${req.version})')

	match req.path {
		'/' {
			return http.ServerResponse{
				status_code: 200
				header:      http.new_header_from_map({
					.content_type: 'text/html; charset=utf-8'
				})
				body:        '<h1>Hello from HTTP/2!</h1><p>This is a V HTTP/2 server.</p>'.bytes()
			}
		}
		'/json' {
			return http.ServerResponse{
				status_code: 200
				header:      http.new_header_from_map({
					.content_type: 'application/json'
				})
				body:        '{"message":"Hello from HTTP/2","protocol":"h2"}'.bytes()
			}
		}
		'/echo' {
			return http.ServerResponse{
				status_code: 200
				header:      http.new_header_from_map({
					.content_type: 'text/plain'
				})
				body:        'Method: ${req.method}\nPath: ${req.path}\nVersion: ${req.version}\n'.bytes()
			}
		}
		else {
			return http.ServerResponse{
				status_code: 404
				header:      http.new_header_from_map({
					.content_type: 'text/plain'
				})
				body:        'Not Found'.bytes()
			}
		}
	}
}

fn main() {
	mut server := http.Server{
		addr:      '0.0.0.0:8080'
		handler:   AppHandler{}
		cert_file: 'cert.pem'
		key_file:  'key.pem'
	}

	println('Starting HTTP/2 server on ${server.addr}')
	println('HTTP/2 is enabled automatically over TLS (ALPN h2)')
	println('Test with: curl -k --http2 https://localhost:8080/')
	println('Press Ctrl+C to stop')

	// listen_and_serve_tls() starts HTTP/2 over TLS automatically
	server.listen_and_serve_tls() or { eprintln('Server error: ${err}') }
}
