module main

import fasthttp

fn handle_request(req fasthttp.HttpRequest) ![]u8 {
	method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()

	if method == 'GET' {
		if path == '/' {
			return home_controller()!
		} else if path.starts_with('/user/') {
			id := path[6..]
			return get_user_controller(id)!
		}
	} else if method == 'POST' {
		if path == '/user' {
			return create_user_controller()!
		}
	}

	return not_found_response()!
}

fn main() {
	mut server := fasthttp.new_server(fasthttp.ServerConfig{
		port:    3000
		handler: handle_request
	}) or {
		eprintln('Failed to create server: ${err}')
		return
	}

	println('Starting fasthttp server on port http://localhost:3000...')

	server.run() or { eprintln('error: ${err}') }
}
