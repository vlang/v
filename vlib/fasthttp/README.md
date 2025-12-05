# fasthttp

The `fasthttp` module is a high-performance HTTP server library for V that provides low-level socket management and non-blocking I/O.

## Features

- **High Performance**: Uses platform-specific I/O multiplexing:
  - `epoll` on Linux for efficient connection handling
  - `kqueue` on macOS for high-performance event notification
- **Non-blocking I/O**: Handles multiple concurrent connections efficiently
- **Simple API**: Easy-to-use request handler pattern
- **Cross-platform**: Supports Linux and macOS

## Installation

The module is part of the standard V library. Import it in your V code:

```v
import fasthttp
```

## Quick Start

Here's a minimal HTTP server example:

```v
import fasthttp

fn handle_request(req fasthttp.HttpRequest) ![]u8 {
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()

	if path == '/' {
		return 'Hello, World!'.bytes()
	}

	return '404 Not Found'.bytes()
}

fn main() {
	mut server := fasthttp.new_server(3000, handle_request) or {
		eprintln('Failed to create server: ${err}')
		return
	}

	println('Server listening on http://localhost:3000')
	server.run() or { eprintln('Server error: ${err}') }
}
```

## API Reference

### `HttpRequest` Struct

Represents an incoming HTTP request.

**Fields:**

- `buffer: []u8` - The raw request buffer containing the complete HTTP request
- `method: Slice` - The HTTP method (GET, POST, etc.)
- `path: Slice` - The request path
- `version: Slice` - The HTTP version (e.g., "HTTP/1.1")
- `client_conn_fd: int` - Internal socket file descriptor

### `Slice` Struct

Represents a slice of the request buffer.

**Fields:**

- `start: int` - Starting index in the buffer
- `len: int` - Length of the slice

**Usage:**

```v
method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
```

## Request Handler Pattern

The handler function receives an `HttpRequest` and must return either:

- `[]u8` - A byte array containing the HTTP response body
- An error if processing failed

The handler should extract method and path information from the request and route accordingly.

**Example:**

```v
fn my_handler(req fasthttp.HttpRequest) ![]u8 {
	method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()

	match method {
		'GET' {
			if path == '/' {
				return 'Home page'.bytes()
			}
		}
		'POST' {
			if path == '/api/data' {
				return 'Data received'.bytes()
			}
		}
		else {}
	}

	return '404 Not Found'.bytes()
}
```

## Response Format

Responses should be returned as byte arrays. The server will send them directly to the client as HTTP response bodies.

```v
// Simple text response
return 'Hello, World!'.bytes()

// HTML response
return '<html><body>Hello</body></html>'.bytes()

// JSON response
return '{"message": "success"}'.bytes()
```

## Example

See the complete example in `examples/fasthttp/` for a more detailed server implementation with multiple routes and controllers.

```sh
./v examples/fasthttp
./examples/fasthttp/fasthttp
```

## Platform Support

- **Linux**: Uses `epoll` for high-performance I/O multiplexing
- **macOS**: Uses `kqueue` for event notification
- **Windows**: Currently not supported

## Performance Considerations

- The `fasthttp` module is designed for high throughput and low latency
- Handler functions should be efficient; blocking operations will affect other connections
- Use goroutines within handlers if you need to perform long-running operations without blocking the I/O loop

## Notes

- HTTP headers are currently not parsed; the entire request is available in the buffer
- Only the request method, path, and version are parsed automatically
- Response status codes and headers must be manually constructed if needed
- The module provides low-level access for maximum control and performance
