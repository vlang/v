module veb

import net.http

fn test_http1_request_head_byte_limit_maps_to_431() {
	assert request_head_error_response(http.RequestHeaderLimitError{}).bytestr() == http_431_response.bytestr()
	assert request_head_error_response(http.HeaderLimitError{}).bytestr() == http_431_response.bytestr()
}

fn test_malformed_request_line_maps_to_400_with_body() {
	// An empty request target or a doubled space before the HTTP version is a
	// client error, so the raw response is a framed 400, never an empty 500.
	// See https://github.com/vlang/v/issues/32333.
	for head in ['GET  HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n',
		'GET /  HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n'] {
		http.parse_request_head_str(head) or {
			resp := http.parse_response(request_head_error_response(err).bytestr()) or {
				panic(err)
			}
			assert resp.status_code == int(http.Status.bad_request)
			assert resp.body == '400 Bad Request'
			assert resp.header.get(.content_length) or { '' } == resp.body.len.str()
			assert resp.header.get(.connection) or { '' } == 'close'
			continue
		}
		assert false, 'accepted malformed request head: ' + head
	}
}

fn test_listening_process_ids_from_lsof_output() {
	assert listening_process_ids_from_lsof_output('42\n123\n') == [42, 123]
	assert listening_process_ids_from_lsof_output('42\ninvalid\n0\n-1\n') == [42]
}
