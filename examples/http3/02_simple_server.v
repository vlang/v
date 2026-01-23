// Simple HTTP/3 Server Example
// Demonstrates basic HTTP/3 server usage with QUIC
import net.http.v3

fn main() {
	println('=== HTTP/3 Server Example ===\n')

	// Note: This example requires OpenSSL and ngtcp2 libraries
	// Install with: brew install openssl ngtcp2 (macOS)
	//            or: apt-get install libssl-dev libngtcp2-dev (Linux)

	// Create HTTP/3 server configuration
	config := v3.ServerConfig{
		addr:                     '0.0.0.0:4433' // HTTP/3 typically uses port 443 or 4433
		max_idle_timeout:         30000          // 30 seconds
		max_udp_payload_size:     1350
		initial_max_data:         1048576
		initial_max_stream_data:  524288
		initial_max_streams_bidi: 100
		// TLS certificate (required for QUIC)
		cert_file: 'server.crt'
		key_file:  'server.key'
	}

	// Create HTTP/3 server with handler
	mut server := v3.new_server(config, handle_request) or {
		eprintln('Failed to create HTTP/3 server: ${err}')
		eprintln('\nNote: HTTP/3 requires OpenSSL and ngtcp2 libraries')
		eprintln('Install with:')
		eprintln('  macOS:  brew install openssl ngtcp2')
		eprintln('  Linux:  apt-get install libssl-dev libngtcp2-dev')
		eprintln('\nAlso need TLS certificate:')
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
	server.listen_and_serve() or { eprintln('Server error: ${err}') }
}

// handle_request processes HTTP/3 requests
fn handle_request(req v3.Request) v3.Response {
	println('[HTTP/3] ${req.method} ${req.path}')

	// Route requests
	match req.path {
		'/' {
			return v3.Response{
				status_code: 200
				headers:     {
					'content-type': 'text/html; charset=utf-8'
					'alt-svc':      'h3=":4433"; ma=86400' // Advertise HTTP/3 support
				}
				body:        html_home()
			}
		}
		'/json' {
			return v3.Response{
				status_code: 200
				headers:     {
					'content-type': 'application/json'
					'alt-svc':      'h3=":4433"; ma=86400'
				}
				body:        json_response()
			}
		}
		'/echo' {
			return v3.Response{
				status_code: 200
				headers:     {
					'content-type': 'text/plain'
					'alt-svc':      'h3=":4433"; ma=86400'
				}
				body:        echo_response(req)
			}
		}
		'/stream' {
			return v3.Response{
				status_code: 200
				headers:     {
					'content-type': 'text/plain'
					'alt-svc':      'h3=":4433"; ma=86400'
				}
				body:        stream_response()
			}
		}
		else {
			return v3.Response{
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

fn echo_response(req v3.Request) []u8 {
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
