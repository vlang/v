module http

import compress.brotli
import compress.gzip
import compress.zlib
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
	mut fixture := ReceiveAllDataFixture{
		parts: parts
	}
	mut capture := ProgressBodyCapture{}
	mut req := Request{
		on_progress_body:   receive_all_data_progress_body_cb
		stop_copying_limit: 6
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
	assert resp.body == 'Wikipe'
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

fn test_receive_all_data_copy_limit_preserves_headers_and_caps_body() {
	body := 'x'.repeat(200000)
	headers := 'HTTP/1.1 200 OK\r\nContent-Length: ${body.len}\r\nContent-Type: text/plain\r\n\r\n'
	for split_headers in [false, true] {
		for limit in [i64(-1), 0, 1, 8192, 16384, 32768, 50000, 100000, 150000, 200000, 250000] {
			mut parts := if split_headers {
				[headers[..headers.len - 2], headers[headers.len - 2..] + body[..1024]]
			} else {
				[headers + body[..1024]]
			}
			for offset := 1024; offset < body.len; offset += 16000 {
				end := if offset + 16000 < body.len { offset + 16000 } else { body.len }
				parts << body[offset..end]
			}
			mut fixture := ReceiveAllDataFixture{ parts: parts }
			mut capture := ProgressBodyCapture{}
			mut req := Request{ stop_copying_limit: limit, on_progress_body: receive_all_data_progress_body_cb, user_ptr: voidptr(&capture) }
			mut content := strings.new_builder(64)
			info := req.receive_all_data_from_cb_in_builder(mut content, voidptr(&fixture), receive_all_data_fixture_cb)!
			resp := parse_received_response(content.str(), info)!
			expected_len := if limit <= 0 || limit > body.len { body.len } else { int(limit) }
			assert resp.status_code == 200
			assert (resp.header.get(.content_type) or { '' }) == 'text/plain'
			assert resp.body == body[..expected_len]
			assert capture.data.bytestr() == body
			assert capture.reads.last() == u64(body.len)
			assert info.reusable
			assert info.has_truncated_body == (expected_len < body.len)
		}
	}
}

fn test_receive_all_data_chunked_copy_limit_counts_decoded_bytes() {
	for limit in [i64(-1), 0, 1, 6, 10, 100] {
		mut fixture := ReceiveAllDataFixture{
			parts: ['HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n',
				'6\r\npedia!\r\n0\r\n\r\n']
		}
		mut req := Request{ stop_copying_limit: limit }
		mut content := strings.new_builder(64)
		info := req.receive_all_data_from_cb_in_builder(mut content, voidptr(&fixture), receive_all_data_fixture_cb)!
		resp := parse_received_response(content.str(), info)!
		body := 'Wikipedia!'
		expected_len := if limit <= 0 || limit > body.len { body.len } else { int(limit) }
		assert resp.body == body[..expected_len]
		assert info.reusable
		assert info.has_truncated_body == (expected_len < body.len)
	}
}

fn check_receive_compressed_body_with_copy_limit(body string, compressed []u8, encoding string, is_chunked bool, limit i64, expected string) ! {
	framing := if is_chunked {
		'Transfer-Encoding: chunked'
	} else {
		'Content-Length: ${compressed.len}'
	}
	headers := 'HTTP/1.1 200 OK\r\n${framing}\r\nContent-Encoding: ${encoding}\r\n\r\n'
	half := compressed.len / 2
	parts := if is_chunked {
		[headers + '${compressed.len:x}\r\n' + compressed[..half].bytestr(),
			compressed[half..].bytestr() + '\r\n0\r\n\r\n']
	} else {
		[headers + compressed[..half].bytestr(), compressed[half..].bytestr()]
	}
	mut fixture := ReceiveAllDataFixture{ parts: parts }
	mut capture := ProgressBodyCapture{}
	mut req := Request{
		stop_copying_limit: limit
		on_progress_body:   receive_all_data_progress_body_cb
		user_ptr:           voidptr(&capture)
	}
	mut content := strings.new_builder(64)
	info := req.receive_all_data_from_cb_in_builder(mut content, voidptr(&fixture), receive_all_data_fixture_cb)!
	resp := parse_received_response(content.str(), info)!
	assert resp.status_code == 200
	assert (resp.header.get(.content_encoding) or { '' }) == encoding
	assert resp.body == expected, 'encoding=${encoding}, chunked=${is_chunked}, limit=${limit}, original=${body.len}'
	assert info.has_truncated_body == (limit > 0 && compressed.len > limit)
	assert capture.data == compressed
	assert capture.reads.last() == u64(compressed.len)
	assert info.reusable
	if limit > 0 {
		assert content.len <= headers.len + limit
		assert resp.body.len <= limit
	}
}

fn test_receive_all_data_copy_limit_preserves_content_encoding() ! {
	for encoding in ['gzip', 'deflate', 'br'] {
		if encoding == 'br' && !brotli.is_available() {
			eprintln('skipping Brotli receive test; libbrotli is not available')
			continue
		}
		for body in ['hello', 'hello'.repeat(200)] {
			compressed := match encoding {
				'gzip' { gzip.compress(body.bytes())! }
				'deflate' { zlib.compress(body.bytes())! }
				else { brotli.compress(body.bytes())! }
			}
			for is_chunked in [true, false] {
				// An ample limit must retain the same content decoding as unlimited reads.
				for limit in [i64(65536), -1, 0] {
					check_receive_compressed_body_with_copy_limit(body, compressed, encoding, is_chunked, limit, body)!
				}
				// The complete encoded body can fit while decompression expands past the limit.
				limit := i64(compressed.len)
				expected := if limit < body.len { body[..int(limit)] } else { body }
				check_receive_compressed_body_with_copy_limit(body, compressed, encoding, is_chunked, limit, expected)!
				// Preserve the existing fallback when a bounded encoded prefix cannot decompress.
				check_receive_compressed_body_with_copy_limit(body, compressed, encoding, is_chunked, 1, compressed[..1].bytestr())!
			}
		}
	}
}
