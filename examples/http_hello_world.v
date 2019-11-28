import http

fn main() {
	srv := http.create_server(handle) or {
		panic(err)
	}
	
	srv.listen(1337) or {
		panic(err)
	}
}

fn handle(req http.ServerRequest, resO http.Response) http.Response {
	mut res := resO
	res.status_code = 200
	res.text = '<h1> Hello World! </h1>'
	res.headers['Content-Type'] = 'text/html'
	return res
}