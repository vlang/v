module main

import strings

const http_ok_response = 'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

const http_created_response = 'HTTP/1.1 201 Created\r\nContent-Type: application/json\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

fn home_controller(params []string) ![]u8 {
	return http_ok_response
}

fn get_users_controller(params []string) ![]u8 {
	return http_ok_response
}

@[direct_array_access; manualfree]
fn get_user_controller(params []string) ![]u8 {
	if params.len == 0 {
		return tiny_bad_request_response
	}
	id := params[0]
	response_body := id

	mut sb := strings.new_builder(200)
	sb.write_string('HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: ')
	sb.write_string(response_body.len.str())
	sb.write_string('\r\nConnection: keep-alive\r\n\r\n')
	sb.write_string(response_body)

	defer {
		unsafe {
			response_body.free()
			params.free()
		}
	}
	return sb
}

fn create_user_controller(params []string) ![]u8 {
	return http_created_response
}
