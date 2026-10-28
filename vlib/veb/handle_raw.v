module veb

import io
import net.http
import os

// RawRequestReader is an in-memory `io.Reader` over raw request bytes, so
// `handle_raw` can feed them through the same buffered wire reader the server
// uses, without opening a socket.
struct RawRequestReader {
	data []u8
mut:
	pos int
}

fn (mut r RawRequestReader) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	mut n := buf.len
	if n > r.data.len - r.pos {
		n = r.data.len - r.pos
	}
	copy(mut buf, r.data[r.pos..r.pos + n])
	r.pos += n
	return n
}

// handle_raw feeds raw HTTP request bytes through the real wire parser
// (`read_request_from_buffered_reader`, the same reader the server accepts
// connections with) and the veb dispatcher, returning the raw response bytes.
// No socket is opened and no listener is started, which makes it usable for
// in-process request tests. Unlike `render_request`, which takes an already
// parsed `http.Request`, this exercises the wire parser too, so malformed
// bytes and body framing behave as they do on the wire. Pipelined requests
// are answered in order and their responses are concatenated. A request that
// fails to parse yields that parse error's response (400-class, via
// `request_read_error_response`) and stops processing, mirroring the server
// closing the connection; route configuration errors yield a 500 response.
pub fn handle_raw[A, X](mut app A, raw []u8) []u8 {
	routes := generate_routes[A, X](app) or { return http_500.bytes() }
	controllers_sorted := check_duplicate_routes_in_controllers[A](app, routes) or {
		return http_500.bytes()
	}
	params := RequestParams{
		controllers_sorted: controllers_sorted
		routes:             &routes
	}
	mut source := RawRequestReader{
		data: raw
	}
	mut reader := io.new_buffered_reader(reader: source, cap: 1024)
	defer {
		unsafe { reader.free() }
	}
	mut out := []u8{}
	for {
		req := read_request_from_buffered_reader(mut reader, 0) or {
			if err is io.Eof {
				break
			}
			out << request_read_error_response(err).bytes()
			break
		}
		mut completed := handle_request_and_route[A, X](mut app, req, -1, params)
		if completed.return_type == .file {
			// Socket transports stream return_file separately from the response.
			// Here the returned buffer must contain those bytes before the next response.
			completed.res.body = os.read_file(completed.return_file) or {
				failure := http.new_response(
					status: .internal_server_error
					body:   '500 Internal Server Error'
					header: http.new_header(http.HeaderConfig{ key: .content_type, value: 'text/plain' },
						key:   .connection
						value: 'close'
					)
				)
				out << failure.bytes()
				break
			}
			completed.res.header.set(.content_length, completed.res.body.len.str())
		}
		completed.res.write_to(mut out)
		if should_close_connection(&completed.req, &completed.res,
			completed.client_wants_to_close)
		{
			break
		}
	}
	return out
}
