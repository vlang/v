// vtest build: !solaris
import json
import picoev
import picohttpparser

const port = 8089

struct Message {
	message string
}

@[inline]
fn json_response() string {
	msg := Message{
		message: 'Hello, World!'
	}
	return json.encode(msg)
}

@[inline]
fn hello_response() string {
	return 'Hello, World!'
}

fn callback(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response) {
	if req.method == 'GET' {
		if req.path == '/t' {
			res.http_ok()
			res.header_server()
			res.header_date()
			res.plain()
			res.body(hello_response())
		} else if req.path == '/j' {
			res.http_ok()
			res.header_server()
			res.header_date()
			res.json()
			res.body(json_response())
		} else {
			res.http_ok()
			res.header_server()
			res.header_date()
			res.html()
			res.body('Hello Picoev!\n')
		}
	} else {
		res.http_405()
	}
	res.end()
}

fn main() {
	println('Starting webserver on http://localhost:${port}/ ...')
	mut server := picoev.new(port: port, cb: callback)!
	server.serve()
}
