module main

fn home_controller() ![]u8 {
	response := 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n\r\nHello, World!'
	return response.bytes()
}

fn get_user_controller(id string) ![]u8 {
	body := 'User ID: ${id}'
	content_length := body.len
	response := 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: ${content_length}\r\n\r\n${body}'
	return response.bytes()
}

fn create_user_controller() ![]u8 {
	body := 'User created successfully'
	content_length := body.len
	response := 'HTTP/1.1 201 Created\r\nContent-Type: text/plain\r\nContent-Length: ${content_length}\r\n\r\n${body}'
	return response.bytes()
}

fn not_found_response() ![]u8 {
	body := '404 Not Found'
	content_length := body.len
	response := 'HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: ${content_length}\r\n\r\n${body}'
	return response.bytes()
}
