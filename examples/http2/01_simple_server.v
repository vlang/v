// Simple HTTP/2 Server Example
// Demonstrates basic HTTP/2 server usage
import net.http.v2

fn main() {
	// Create server configuration
	config := v2.ServerConfig{
		addr:                   '0.0.0.0:8080'
		max_concurrent_streams: 100
		initial_window_size:    65535
		max_frame_size:         16384
	}

	// Create server with handler
	mut server := v2.new_server(config, handle_request) or {
		eprintln('Failed to create server: ${err}')
		return
	}

	println('Starting HTTP/2 server on ${config.addr}')
	println('Test with: curl --http2-prior-knowledge http://localhost:8080/')
	println('Press Ctrl+C to stop')

	// Start server (blocks)
	server.listen_and_serve() or { eprintln('Server error: ${err}') }
}

// handle_request processes HTTP/2 requests
fn handle_request(req v2.ServerRequest) v2.ServerResponse {
	println('Received: ${req.method} ${req.path}')

	// Route requests
	match req.path {
		'/' {
			return v2.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'text/html; charset=utf-8'
				}
				body:        '<h1>Hello from HTTP/2!</h1><p>This is a V HTTP/2 server.</p>'.bytes()
			}
		}
		'/json' {
			return v2.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'application/json'
				}
				body:        '{"message":"Hello from HTTP/2","protocol":"h2"}'.bytes()
			}
		}
		'/echo' {
			return v2.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'text/plain'
				}
				body:        'Method: ${req.method}\nPath: ${req.path}\n'.bytes()
			}
		}
		else {
			return v2.ServerResponse{
				status_code: 404
				headers:     {
					'content-type': 'text/plain'
				}
				body:        'Not Found'.bytes()
			}
		}
	}
}
