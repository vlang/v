module httptest

import net.http

struct HelloHandler {}

fn (h HelloHandler) handle(req http.Request) http.Response {
	return http.Response{
		status_code:  200
		status_msg:   'OK'
		body:         'hello test'
		http_version: '1.1'
	}
}

fn test_server_roundtrip() {
	mut ts := new_server(HelloHandler{})!
	defer {
		ts.close()
	}
	assert ts.url.starts_with('http://127.0.0.1:')
	resp := http.get('${ts.url}/hi')!
	assert resp.status_code == 200
	assert resp.body == 'hello test'
}
