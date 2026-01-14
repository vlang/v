module main

import fasthttp

fn handle_request(req fasthttp.HttpRequest) !fasthttp.HttpResponse {
	method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()

	if method == 'GET' {
		if path == '/' {
			return fasthttp.HttpResponse{
				content: home_controller()!
			}
		} else if path.starts_with('/user/') {
			id := path[6..]
			return fasthttp.HttpResponse{
				content: get_user_controller(id)!
			}
		}
	} else if method == 'POST' {
		if path == '/user' {
			return fasthttp.HttpResponse{
				content: create_user_controller()!
			}
		}
	}

	return fasthttp.HttpResponse{
		content: not_found_response()!
	}
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
