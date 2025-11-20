/*
$if darwin {
	import fasthttp
	import time
	import os

	const text = 'hello world'

	// This is your custom application logic. The server will call this function
	// for each incoming request.
	fn request_handler(req fasthttp.HttpRequest) ![]u8 {
		s := req.buffer.bytestr()
		_ = s
		path := req.path.str()
		// println("REQUEST HANDLER() $path")
		// println('Handling request for path: "${path}"')

		// return []u8('<b>Hello from the IO thread!</b>')
		// return '<b>Hello from the IO thread!</b>'.bytes()

		match path {
			'/' {
				return text.bytes() //'<b>Hello from the IO thread!</b>'.bytes()
			}
			'/sleep' {
				// This code will run in a worker thread because the server
				// is configured to offload requests for this path.
				time.sleep(5 * time.second)
				return '<b>Hello from the worker thread after a 5s sleep!</b>'.bytes()
			}
			else {
				return '<b>404 Not Found</b>'.bytes()
			}
		}
	}

	fn test_lol() {
		// Create a new server instance on port 8092, passing our handler function.
		mut server := fasthttp.new_server(8092, request_handler) or {
			eprintln('Failed to create server: ${err}')
			return
		}

		// Start the server's event loop. This function will block indefinitely.
		server.run() or { eprintln('Server failed to run: ${err}') }
	}
}

fn test_x() {
	assert true
}
*/
