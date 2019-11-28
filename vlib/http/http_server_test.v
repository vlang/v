import http

fn test_404() {
	srv := http.create_server(serve) or {
		panic(err)
	}
	go listen(srv)
	res := http.get('http://127.0.0.1:1337/nonexistantpage1234') or {
		panic(err)
	}
	assert res.status_code == 404
}

fn serve(req http.Request, res http.Response) http.Response {
	return res
}

fn listen(srv http.HttpServer) {
	srv.listen(1337) or {
		panic(err)
	}
}