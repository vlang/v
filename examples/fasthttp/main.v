module main

import fasthttp

// handle is a fasthttp append handler: it appends the complete raw HTTP response
// (status line + headers + body) into the reused `out` buffer and returns a Step.
// No response object is allocated per request.
fn handle(req fasthttp.HttpRequest, mut out []u8, worker_state voidptr, mut ctl fasthttp.ResponseControl) fasthttp.Step {
	method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()

	if method == 'GET' {
		if path == '/' {
			home_controller(mut out)
			return .done
		} else if path.starts_with('/user/') {
			get_user_controller(mut out, path[6..])
			return .done
		}
	} else if method == 'POST' {
		if path == '/user' {
			create_user_controller(mut out)
			return .done
		}
	}

	not_found_response(mut out)
	return .done
}

fn main() {
	mut server := fasthttp.new_server(fasthttp.ServerConfig{
		port:           3000
		append_handler: handle
	}) or {
		eprintln('Failed to create server: ${err}')
		return
	}

	println('Starting fasthttp server on http://localhost:3000 ...')

	server.run() or { eprintln('error: ${err}') }
}
