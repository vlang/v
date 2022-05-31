module main

import net.http { CommonHeader, Request, Response, Server }

struct ExampleHandler {}

fn (h ExampleHandler) handle(req Request) Response {
	mut res := Response{
		header: http.new_header_from_map({
			CommonHeader.content_type: 'text/plain'
		})
	}
	mut status_code := 200
	res.body = match req.url {
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
	res.status_code = status_code
	return res
}

fn main() {
	mut server := Server{
		handler: ExampleHandler{}
	}
	server.listen_and_serve()?
}
