module main

// The controllers append their raw HTTP response straight into the connection's
// reused write buffer `out` (keep-alive; the server batches pipelined responses).

fn home_controller(mut out []u8) {
	out << 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 13\r\nConnection: keep-alive\r\n\r\nHello, World!'.bytes()
}

fn get_user_controller(mut out []u8, id string) {
	body := 'User ID: ${id}'
	out << 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: ${body.len}\r\nConnection: keep-alive\r\n\r\n${body}'.bytes()
}

fn create_user_controller(mut out []u8) {
	body := 'User created successfully'
	out << 'HTTP/1.1 201 Created\r\nContent-Type: text/plain\r\nContent-Length: ${body.len}\r\nConnection: keep-alive\r\n\r\n${body}'.bytes()
}

fn not_found_response(mut out []u8) {
	body := '404 Not Found'
	out << 'HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: ${body.len}\r\nConnection: keep-alive\r\n\r\n${body}'.bytes()
}
