module main

import vanilla.http_server

fn test_handle_request_get_home() {
	req_buffer := 'GET / HTTP/1.1\r\n\r\n'.bytes()
	response := handle_request(req_buffer, -1) or { panic(err) }
	assert response == http_ok_response
}

fn test_handle_request_get_user() {
	req_buffer := 'GET /user/123 HTTP/1.1\r\n\r\n'.bytes()
	response := handle_request(req_buffer, -1) or { panic(err) }
	assert response == 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 3\r\nConnection: keep-alive\r\n\r\n123'.bytes()
}

fn test_handle_request_post_user() {
	req_buffer := 'POST /user HTTP/1.1\r\nContent-Length: 0\r\n\r\n'.bytes()
	response := handle_request(req_buffer, -1) or { panic(err) }
	assert response == http_created_response
}

fn test_handle_request_bad_request() {
	req_buffer := 'INVALID / HTTP/1.1\r\n\r\n'.bytes()
	response := handle_request(req_buffer, -1) or { panic(err) }
	assert response == http_server.tiny_bad_request_response
}
