module main

import net.http

struct ExampleHandler {}

fn (h ExampleHandler) handle(req http.ServerRequest) http.ServerResponse {
	mut status_code := 200
	body := match req.path {
		'/foo' {
			'bar\n'
		}
		'/hello' {
			'world\n'
		}
		'/' {
			'foo\nhello\n'
		}
		else {
			status_code = 404
			'Not found\n'
		}
	}
	return http.ServerResponse{
		status_code: status_code
		header: http.new_header_from_map({
			.content_type: 'text/plain'
		})
		body: body.bytes()
	}
}

fn main() {
	mut server := http.Server{
		handler: ExampleHandler{}
	}
	server.listen_and_serve()
}
