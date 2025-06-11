module main

import vanilla.http_server
import vanilla.request_parser

fn handle_request(req_buffer []u8, client_conn_fd int) ![]u8 {
	req := request_parser.decode_http_request(req_buffer)!

	method := unsafe { tos(&req.buffer[req.method.start], req.method.len) }
	path := unsafe { tos(&req.buffer[req.path.start], req.path.len) }

	if method == 'GET' {
		if path == '/' {
			return home_controller([])
		} else if path.starts_with('/user/') {
			id := path[6..]
			return get_user_controller([id])
		}
	} else if method == 'POST' {
		if path == '/user' {
			return create_user_controller([])
		}
	}

	return http_server.tiny_bad_request_response
}

fn main() {
	mut vanilla := http_server.Server{
		request_handler: handle_request
		port:            3001
	}

	vanilla.run()
}
