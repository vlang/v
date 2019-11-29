import http

fn test_simple() {
	srv := http.create_server(handle) or {
		panic(err)
	}
	go listen(srv)
	res := http.get('http://127.0.0.1:1337') or {
		panic(err)
	}
	assert res.status_code == 200
	srv.free() or {
		panic(err)
	}
}

fn handle(req http.ServerRequest, resO http.Response) http.Response {
	mut res := resO // Make a clone of resO to edit
	res.status_code = 200
	res.text = 'hello test'
	return res // Return our edited clone to be sent
}

fn listen(srv http.HttpServer) {
	srv.listen(1337) or {
		panic(err)
	}
}