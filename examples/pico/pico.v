import json
import picoev
import picohttpparser

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


fn callback(req picohttpparser.Request, res mut picohttpparser.Response) {
	if picohttpparser.cmpn(req.method, 'GET ', 4) {
		if picohttpparser.cmp(req.path, '/t') {
			res.http_ok().header_server().header_date().plain().body(hello_response())
		}
		else if picohttpparser.cmp(req.path, '/j') {
			res.http_ok().header_server().header_date().json().body(json_response())
		}
		else {
			res.http_404()
		}
	}
	else {
		res.http_405()
	}
}

fn main() {
	picoev.new(8088, &callback).serve()
}
