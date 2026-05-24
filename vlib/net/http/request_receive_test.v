module http

import io
import net
import strings

fn receive_all_data_timeout_cb(_ voidptr, _ &u8, _ int) !int {
	return error_with_code('read timed out', net.err_timed_out_code)
}

fn receive_all_data_eof_cb(_ voidptr, _ &u8, _ int) !int {
	return io.Eof{}
}

struct ReceiveAllDataFixture {
	parts []string
mut:
	index int
}

fn receive_all_data_fixture_cb(con voidptr, buf &u8, bufsize int) !int {
	mut fixture := unsafe { &ReceiveAllDataFixture(con) }
	if fixture.index >= fixture.parts.len {
		return io.Eof{}
	}
	part := fixture.parts[fixture.index].bytes()
	fixture.index++
	assert part.len <= bufsize
	mut out := unsafe { buf.vbytes(bufsize) }
	return copy(mut out, part)
}

struct ProgressBodyCapture {
mut:
	data     []u8
	reads    []u64
	expected []u64
	statuses []int
}

fn receive_all_data_progress_body_cb(request &Request, chunk []u8, body_so_far u64, expected_size u64, status_code int) ! {
	mut capture := unsafe { &ProgressBodyCapture(request.user_ptr) }
	capture.data << chunk
	capture.reads << body_so_far
	capture.expected << expected_size
	capture.statuses << status_code
}

fn test_receive_all_data_from_cb_in_builder_propagates_non_eof_errors() {
	mut req := Request{}
	mut content := strings.new_builder(64)
	req.receive_all_data_from_cb_in_builder(mut content, unsafe { nil },
		receive_all_data_timeout_cb) or {
		assert err.code() == net.err_timed_out_code
		return
	}
	panic('expected a timeout error')
}

fn test_receive_all_data_from_cb_in_builder_stops_on_eof() {
	mut req := Request{}
	mut content := strings.new_builder(64)
	req.receive_all_data_from_cb_in_builder(mut content, unsafe { nil }, receive_all_data_eof_cb) or {
		panic('unexpected error: ${err}')
	}
	assert content.str() == ''
}

fn test_receive_all_data_from_cb_in_builder_dechunks_progress_body_and_parses_truncated_chunked_response() {
	parts := [
		'HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nContent-Type: text/plain\r\n\r\n4\r\nWi',
		'ki\r\n6',
		'\r\nped',
		'ia!\r\n0\r',
		'\n\r\n',
	]
	full_response := parts.join('')
	mut fixture := ReceiveAllDataFixture{
		parts: parts
	}
	mut capture := ProgressBodyCapture{}
	mut req := Request{
		on_progress_body:   receive_all_data_progress_body_cb
		stop_copying_limit: i64(full_response.len - 1)
		user_ptr:           voidptr(&capture)
	}
	mut content := strings.new_builder(64)
	response_info := req.receive_all_data_from_cb_in_builder(mut content, voidptr(&fixture),
		receive_all_data_fixture_cb)!
	assert response_info.is_chunked_transfer
	assert response_info.has_truncated_body
	assert capture.data.bytestr() == 'Wikipedia!'
	assert capture.reads == [u64(2), 4, 7, 10]
	assert capture.expected == [u64(0), 0, 0, 0]
	assert capture.statuses == [200, 200, 200, 200]
	resp := parse_received_response(content.str(), response_info)!
	assert resp.status_code == 200
	assert resp.body == ''
}

fn test_receive_all_data_from_cb_in_builder_errors_on_premature_eof_with_content_length() {
	mut fixture := ReceiveAllDataFixture{
		parts: [
			'HTTP/1.1 200 OK\r\nContent-Length: 10\r\nContent-Type: text/plain\r\n\r\nhello',
		]
	}
	mut req := Request{}
	mut content := strings.new_builder(64)
	req.receive_all_data_from_cb_in_builder(mut content, voidptr(&fixture),
		receive_all_data_fixture_cb) or {
		assert err.msg().contains('response body ended early')
		assert err.msg().contains('5 of 10 bytes')
		return
	}
	panic('expected an early EOF error for a truncated fixed-length response')
}

fn test_receive_all_data_from_cb_in_builder_errors_on_incomplete_chunked_response() {
	mut fixture := ReceiveAllDataFixture{
		parts: [
			'HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nContent-Type: text/plain\r\n\r\n4\r\nWi',
			'ki\r\n6\r\nped',
		]
	}
	mut req := Request{}
	mut content := strings.new_builder(64)
	req.receive_all_data_from_cb_in_builder(mut content, voidptr(&fixture),
		receive_all_data_fixture_cb) or {
		assert err.msg() == 'http.request: incomplete chunked response'
		return
	}
	panic('expected an early EOF error for an incomplete chunked response')
}
