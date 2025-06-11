module main

import vanilla.http_server
import vanilla.request_parser

fn handle_request(req_buffer []u8, client_conn_fd int) ![]u8 {
	req := request_parser.decode_http_request(req_buffer)!
	method := request_parser.slice_to_string(req.buffer, req.method)
	path := request_parser.slice_to_string(req.buffer, req.path)

	match method {
		'GET' {
			match path {
				'/' {
					return home_controller([])
				}
				'/users' {
					return get_users_controller([])
				}
				else {
					if path.starts_with('/user/') {
						id := path[6..]
						return get_user_controller([id])
					}
					return http_server.tiny_bad_request_response
				}
			}
		}
		'POST' {
			if path == '/user' {
				return create_user_controller([])
			}
			return http_server.tiny_bad_request_response
		}
		else {
			return http_server.tiny_bad_request_response
		}
	}

	return http_server.tiny_bad_request_response
}

fn main() {
	mut server := http_server.Server{
		port:            3000
		request_handler: handle_request
	}
	server.run()
}
