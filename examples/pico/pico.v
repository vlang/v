import json
import picoev
import picohttpparser

const (
	port = 8088
)

struct Message {
	message string
}

[inline]
fn json_response() string {
	msg := Message{
		message: 'Hello, World!'
	}
	return json.encode(msg)
}

[inline]
fn hello_response() string {
	return 'Hello, World!'
}

fn callback(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response) {
	if picohttpparser.cmpn(req.method, 'GET ', 4) {
		if picohttpparser.cmp(req.path, '/t') {
			res.http_ok()
			res.header_server()
			res.header_date()
			res.plain()
			res.body(hello_response())
		} else if picohttpparser.cmp(req.path, '/j') {
			res.http_ok()
			res.header_server()
			res.header_date()
			res.json()
			res.body(json_response())
		} else {
			res.http_404()
		}
	} else {
		res.http_405()
	}
	res.end()
}

fn main() {
	println('Starting webserver on http://127.0.0.1:${port}/ ...')
	picoev.new(port: port, cb: &callback).serve()
}
