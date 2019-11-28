import http

fn main() {
	srv := http.create_server(handle) or {
		panic(err)
	}
	
	srv.listen(1337) or {
		panic(err)
	}
}

fn handle(req http.Request, res http.Response) http.Response {
	return res
}