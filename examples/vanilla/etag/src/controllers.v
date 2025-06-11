module main

import strings
import vanilla.http_server
import vanilla.request_parser
import crypto.md5

const not_modified_responsense = 'HTTP/1.1 304 Not Modified\r\n\r\n'.bytes()

fn generate_etag(content []u8) []u8 {
	return md5.sum(content)
}

const http_ok_response = 'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

const http_created_response = 'HTTP/1.1 201 Created\r\nContent-Type: application/json\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

fn home_controller(params []string) ![]u8 {
	return http_ok_response
}

fn get_users_controller(params []string) ![]u8 {
	return http_ok_response
}

@[direct_array_access; manualfree]
fn get_user_controller(params []string, req request_parser.HttpRequest) ![]u8 {
	if params.len == 0 {
		return http_server.tiny_bad_request_response
	}
	id := params[0]
	response_body := id.bytes() // Convert to []u8 for hashing

	// Generate an ETag for the response body.
	etag := generate_etag(response_body)
	etag_str := etag.hex() // convert to a hexadecimal string for header usage

	// Extract the If-None-Match header from the request.
	if_none_match := req.get_header_value_slice('If-None-Match')
	if if_none_match != none {
		// Compare the provided ETag with the generated one.
		if unsafe { vmemcmp(&req.buffer[if_none_match.start], etag_str.str, etag_str.len) } == 0 {
			// If they match, return a 304 Not Modified response.
			return not_modified_responsense
		}
	}

	// Build the full response including the new ETag header.
	mut sb := strings.new_builder(200)
	body_str := unsafe { tos(response_body.data, response_body.len) }

	// Write the response with ETag.
	sb.write_string('HTTP/1.1 200 OK\r\n')
	sb.write_string('Content-Type: text/plain\r\n')
	sb.write_string('ETag: ' + etag_str + '\r\n')
	sb.write_string('Content-Length: ' + body_str.len.str() + '\r\n')
	sb.write_string('Access-Control-Allow-Origin: *\r\n\r\n')
	sb.write_string(body_str)

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
