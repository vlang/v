// HTTP/3 Server Example
// Demonstrates HTTP/3 server using the unified net.http API.
// Uses listen_and_serve_all() with enable_h3 for HTTP/1.1 + HTTP/2 + HTTP/3.
//
// Requirements:
// 1. TLS 1.3 certificates
// 2. QUIC support (OpenSSL 3.0+ or compatible)
//
// To generate test certificates:
//   openssl req -x509 -newkey rsa:2048 -nodes \
//     -keyout server.key -out server.crt -days 365 \
//     -subj "/CN=localhost"
module main

import net.http

struct AppHandler {}

fn (h AppHandler) handle(req http.ServerRequest) http.ServerResponse {
	println('[${req.version}] ${req.method} ${req.path}')

	match req.path {
		'/' {
			return http.ServerResponse{
				status_code: 200
				header:      http.new_header_from_map({
					.content_type: 'text/html; charset=utf-8'
				})
				body:        html_home().bytes()
			}
		}
		'/json' {
			return http.ServerResponse{
				status_code: 200
				header:      http.new_header_from_map({
					.content_type: 'application/json'
				})
				body:        json_response().bytes()
			}
		}
		'/echo' {
			return http.ServerResponse{
				status_code: 200
				header:      http.new_header_from_map({
					.content_type: 'text/plain'
				})
				body:        echo_response(req).bytes()
			}
		}
		'/stream' {
			return http.ServerResponse{
				status_code: 200
				header:      http.new_header_from_map({
					.content_type: 'text/plain'
				})
				body:        stream_response().bytes()
			}
		}
		else {
			return http.ServerResponse{
				status_code: 404
				header:      http.new_header_from_map({
					.content_type: 'text/plain'
				})
				body:        '404 Not Found\n'.bytes()
			}
		}
	}
}

fn main() {
	println('=== HTTP/3 Server Example ===\n')

	mut server := http.Server{
		addr:      '0.0.0.0:8080'
		tls_addr:  ':4433'
		h3_addr:   ':4433'
		handler:   AppHandler{}
		cert_file: 'server.crt'
		key_file:  'server.key'
		enable_h3: true
	}

	println('HTTP/3 server starting...')
	println('  HTTP/1.1: http://localhost:8080/')
	println('  HTTP/2:   https://localhost:4433/ (TLS)')
	println('  HTTP/3:   https://localhost:4433/ (QUIC)')
	println('Test with: curl --http3 https://localhost:4433/')
	println('Press Ctrl+C to stop\n')

	// listen_and_serve_all() starts all protocols with the same handler
	server.listen_and_serve_all() or { eprintln('Server error: ${err}') }
}

fn html_home() string {
	return '<!DOCTYPE html>
<html>
<head>
    <title>HTTP/3 Server</title>
    <style>
        body { font-family: Arial, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }
        h1 { color: #333; }
        .protocol { background: #e8f5e9; padding: 10px; border-radius: 5px; }
        .endpoints { background: #f5f5f5; padding: 15px; border-radius: 5px; margin-top: 20px; }
        code { background: #f0f0f0; padding: 2px 6px; border-radius: 3px; }
    </style>
</head>
<body>
    <h1>HTTP/3 Server (QUIC)</h1>
    <div class="protocol">
        <strong>Protocol:</strong> HTTP/3 over QUIC<br>
        <strong>Features:</strong> Multiplexing, 0-RTT, UDP-based
    </div>
    <div class="endpoints">
        <h2>Available Endpoints:</h2>
        <ul>
            <li><code>GET /</code> - This page</li>
            <li><code>GET /json</code> - JSON response</li>
            <li><code>GET /echo</code> - Echo request info</li>
            <li><code>GET /stream</code> - Stream data example</li>
        </ul>
    </div>
</body>
</html>'
}

fn json_response() string {
	return '{
  "message": "Hello from HTTP/3!",
  "protocol": "h3",
  "transport": "QUIC",
  "features": [
    "Multiplexing",
    "0-RTT Connection",
    "UDP-based",
    "Built-in Encryption"
  ]
}'
}

fn echo_response(req http.ServerRequest) string {
	mut response := 'Echo Response\n'
	response += '===================\n\n'
	response += 'Method: ${req.method}\n'
	response += 'Path: ${req.path}\n'
	response += 'Version: ${req.version}\n'
	response += 'Stream ID: ${req.stream_id}\n'
	response += '\nHeaders:\n'
	for key in req.header.keys() {
		value := req.header.get_custom(key) or { '' }
		response += '  ${key}: ${value}\n'
	}
	response += '\nBody Length: ${req.body.len} bytes\n'
	return response
}

fn stream_response() string {
	mut response := 'HTTP/3 Stream Data\n'
	response += '==================\n\n'
	response += 'This demonstrates HTTP/3 multiplexing.\n'
	response += 'Multiple streams can be sent concurrently.\n\n'
	for i in 1 .. 11 {
		response += 'Stream chunk ${i}/10\n'
	}
	return response
}
