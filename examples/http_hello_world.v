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
	ua := req.headers['User-Agent']
	res.text = '<h1> Hello World! </h1> <p> Your user agent: $ua</p>'
	res.headers['Content-Type'] = 'text/html'
	return res
}