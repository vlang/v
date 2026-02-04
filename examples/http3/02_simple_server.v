// Simple HTTP/3 Server Example
// Demonstrates basic HTTP/3 server structure and usage with QUIC
//
// Note: This example requires full QUIC implementation:
// 1. QUIC library with OpenSSL 3.0+ QUIC support
// 2. TLS 1.3 certificates
// 3. UDP socket handling
//
// Current status: QUIC implementation is in progress.
// This example shows the API structure and will gracefully handle missing dependencies.
//
// To generate test certificates:
//   openssl req -x509 -newkey rsa:2048 -nodes \
//     -keyout server.key -out server.crt -days 365 \
//     -subj "/CN=localhost"
import net.http.v3

fn main() {
	println('=== HTTP/3 Server Example ===\n')

	// Create HTTP/3 server configuration
	mut config := v3.ServerConfig{
		addr:             '0.0.0.0:4433' // HTTP/3 typically uses port 443 or 4433
		max_idle_timeout: 30000          // 30 seconds
		max_data:         10485760       // 10MB
		max_stream_data:  1048576        // 1MB
		// TLS certificate (required for QUIC)
		cert_file: 'server.crt'
		key_file:  'server.key'
	}

	// Set request handler
	config.handler = handle_request

	// Create HTTP/3 server
	mut server := v3.new_server(config) or {
		eprintln('Failed to create HTTP/3 server: ${err}')
		eprintln('\nThis is expected - HTTP/3 requires full QUIC implementation.')
		eprintln('The error message provides details about what is needed.')
		eprintln('\nNote: OpenSSL 3.0+ with QUIC support is required.')
		eprintln('      Most systems currently have OpenSSL 1.1.1 which does not support QUIC.')
		eprintln('\nTo generate test certificates:')
		eprintln('  openssl req -x509 -newkey rsa:2048 -nodes \\')
		eprintln('    -keyout server.key -out server.crt -days 365 \\')
		eprintln('    -subj "/CN=localhost"')
		return
	}

	println('HTTP/3 server listening on ${config.addr}')
	println('Protocol: HTTP/3 (QUIC)')
	println('Test with: curl --http3 https://localhost:4433/')
	println('Press Ctrl+C to stop\n')

	// Start server (blocks)
	server.start() or { eprintln('Server error: ${err}') }
}

// handle_request processes HTTP/3 requests
fn handle_request(req v3.ServerRequest) v3.ServerResponse {
	println('[HTTP/3] ${req.method} ${req.path}')

	// Route requests
	match req.path {
		'/' {
			return v3.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'text/html; charset=utf-8'
					'alt-svc':      'h3=":4433"; ma=86400' // Advertise HTTP/3 support
				}
				body:        html_home()
			}
		}
		'/json' {
			return v3.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'application/json'
					'alt-svc':      'h3=":4433"; ma=86400'
				}
				body:        json_response()
			}
		}
		'/echo' {
			return v3.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'text/plain'
					'alt-svc':      'h3=":4433"; ma=86400'
				}
				body:        echo_response(req)
			}
		}
		'/stream' {
			return v3.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'text/plain'
					'alt-svc':      'h3=":4433"; ma=86400'
				}
				body:        stream_response()
			}
		}
		else {
			return v3.ServerResponse{
				status_code: 404
				headers:     {
					'content-type': 'text/plain'
				}
				body:        '404 Not Found\n'.bytes()
			}
		}
	}
}

fn html_home() []u8 {
	html := '<!DOCTYPE html>
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
    <h1>🚀 HTTP/3 Server (QUIC)</h1>
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
	return html.bytes()
}

fn json_response() []u8 {
	json := '{
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
	return json.bytes()
}

fn echo_response(req v3.ServerRequest) []u8 {
	mut response := 'HTTP/3 Echo Response\n'
	response += '===================\n\n'
	response += 'Method: ${req.method}\n'
	response += 'Path: ${req.path}\n'
	response += 'Stream ID: ${req.stream_id}\n'
	response += '\nHeaders:\n'
	for key, value in req.headers {
		response += '  ${key}: ${value}\n'
	}
	response += '\nBody Length: ${req.body.len} bytes\n'
	return response.bytes()
}

fn stream_response() []u8 {
	mut response := 'HTTP/3 Stream Data\n'
	response += '==================\n\n'
	response += 'This demonstrates HTTP/3 multiplexing.\n'
	response += 'Multiple streams can be sent concurrently.\n\n'
	for i in 1 .. 11 {
		response += 'Stream chunk ${i}/10\n'
	}
	return response.bytes()
}
