module http

import compress.brotli
import compress.deflate
import compress.gzip
import compress.zlib

fn test_normalize_server_response_preserves_unassigned_three_digit_status() {
	mut resp := Response{
		status_code: 299
	}
	normalize_server_response(mut resp, Request{})
	assert resp.status_code == 299
	assert resp.status_msg == ''
}

fn test_normalize_server_response_defaults_zero_status_despite_reason_phrase() {
	mut resp := Response{
		status_msg: 'custom reason without a status'
	}
	normalize_server_response(mut resp, Request{})
	assert resp.status_code == 200
	assert resp.status_msg == 'OK'
}

fn test_response_bytestr_1() {
	resp := new_response(
		status: .ok
		body:   'Foo'
	)
	assert resp.bytestr() == 'HTTP/1.1 200 OK\r\n' + 'Content-Length: 3\r\n' + '\r\n' + 'Foo'
}

fn test_response_bytestr_2() {
	resp := new_response(
		status: .found
		body:   'Foo'
		header: new_header(key: .location, value: '/')
	)
	lines := resp.bytestr().split_into_lines()
	assert lines[0] == 'HTTP/1.1 302 Found'
	// header order is not guaranteed
	check_headers(['Location: /', 'Content-Length: 3'], lines[1..3])!
	assert lines[3] == ''
	assert lines[4] == 'Foo'
}

// check_headers is a helper function for asserting all expected headers
// are found because rendered header order is not guaranteed. The check
// is O(n^2) which is fine for small lists.
fn check_headers(expected []string, found []string) ! {
	assert expected.len == found.len
	for header in expected {
		if !found.contains(header) {
			return error('expected header "${header}" not in ${found}')
		}
	}
}

fn test_parse_response() {
	content := 'HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\nFoo'
	x := parse_response(content)!
	assert x.http_version == '1.1'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.header.contains(.content_length)
	assert x.header.get(.content_length)? == '3'
	assert x.body == 'Foo'
}

fn test_parse_response_without_reason_phrase() {
	// RFC 9112 §4: the reason-phrase is optional, and some servers omit the
	// trailing SP before it as well (`HTTP/1.1 200`).
	content := 'HTTP/1.1 200\r\nContent-Length: 3\r\n\r\nFoo'
	x := parse_response(content)!
	assert x.http_version == '1.1'
	assert x.status_code == 200
	assert x.status_msg == ''
	assert x.header.contains(.content_length)
	assert x.header.get(.content_length)? == '3'
	assert x.body == 'Foo'
}

fn test_parse_response_with_empty_reason_phrase() {
	// trailing SP present, but the reason phrase itself is empty
	content := 'HTTP/1.1 200 \r\nContent-Length: 3\r\n\r\nFoo'
	x := parse_response(content)!
	assert x.http_version == '1.1'
	assert x.status_code == 200
	assert x.status_msg == ''
	assert x.header.contains(.content_length)
	assert x.header.get(.content_length)? == '3'
	assert x.body == 'Foo'
}

fn test_parse_response_with_cookies() {
	cookie_id := 'v_is_best'
	content := 'HTTP/1.1 200 OK\r\nSet-Cookie: id=${cookie_id}\r\nContent-Length: 3\r\n\r\nFoo'
	mut x := parse_response(content)!
	assert x.http_version == '1.1'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.header.contains(.content_length)
	assert x.header.get(.content_length)? == '3'
	assert x.body == 'Foo'
	response_cookie := x.cookies()
	assert response_cookie[0].str() == 'id=${cookie_id}'

	// cookie has Base64 encoding info, ending with '=='
	cookie_base64 := 'Ln0kBnAaAyYFQ8lH7d5J8Y5w1/iyDRpj6d0nBLTbBUMbtEyPD32rPvpApsvxhLJWlkHuHT3KYL0g/xNBxC9od5tMFAgurLxKdRd5lZ6Pd7W+SllkbsXmUA=='
	content_cookie_base64 := 'HTTP/1.1 200 OK\r\nSet-Cookie: enctoken=${cookie_base64}; path=/; secure; SameSite=None\r\nContent-Length: 3\r\n\r\nFoo'
	x = parse_response(content_cookie_base64)!
	assert x.http_version == '1.1'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.header.contains(.content_length)
	assert x.header.get(.content_length)? == '3'
	assert x.body == 'Foo'
	response_cookie_base64 := x.cookies()
	assert response_cookie_base64[0].str().split(';')[0] == 'enctoken=${cookie_base64}'
}

fn test_parse_response_cookie_expires() {
	content := 'HTTP/1.1 200 OK\r\nSet-Cookie: a=b; Expires=Tue, 10 Nov 2009 23:00:00 GMT\r\nSet-Cookie: c=d; expires=Tue, 10-Nov-2009 23:00:00 GMT\r\nSet-Cookie: e=f; expires=bogus\r\nContent-Length: 0\r\n\r\n'
	x := parse_response(content)!
	cookies := x.cookies()
	assert cookies.len == 3
	assert cookies[0].expires.unix() == 1257894000
	assert cookies[0].raw_expires == 'Tue, 10 Nov 2009 23:00:00 GMT'
	assert cookies[1].expires.unix() == 1257894000
	assert cookies[2].expires.year == 0
	assert cookies[2].raw_expires == 'bogus'
	assert 'expires=bogus' in cookies[2].unparsed
}

fn test_parse_response_cookie_expires_boundaries_and_invalid_dates() {
	for expires in ['Tue, 31 Apr 2009 23:00:00 GMT', 'Sun, 29 Feb 2009 23:00:00 GMT',
		'Sun, 31 Dec 1600 23:00:00 GMT'] {
		content := 'HTTP/1.1 200 OK\r\nSet-Cookie: a=b; Expires=${expires}\r\nContent-Length: 0\r\n\r\n'
		response := parse_response(content)!
		cookies := response.cookies()
		assert cookies.len == 1
		assert cookies[0].expires.year == 0
		assert cookies[0].raw_expires == expires
		assert 'Expires=${expires}' in cookies[0].unparsed
	}
	content := 'HTTP/1.1 200 OK\r\nSet-Cookie: a=b; Expires=Mon, 01 Jan 1601 00:00:00 GMT\r\nContent-Length: 0\r\n\r\n'
	response := parse_response(content)!
	cookies := response.cookies()
	assert cookies.len == 1
	assert cookies[0].expires.year == 1601
	assert 'Expires=Mon, 01 Jan 1601 00:00:00 GMT' !in cookies[0].unparsed
}

fn test_parse_response_cookie_expires_duplicate_attributes() {
	valid := 'Tue, 10 Nov 2009 23:00:00 GMT'
	for invalid in ['bogus', 'Sun, 31 Dec 1600 23:00:00 GMT'] {
		content := 'HTTP/1.1 200 OK\r\nSet-Cookie: a=b; Expires=${valid}; expires=${invalid}\r\nContent-Length: 0\r\n\r\n'
		response := parse_response(content)!
		cookies := response.cookies()
		assert cookies.len == 1
		assert cookies[0].expires.unix() == 1257894000
		assert cookies[0].raw_expires == invalid
		assert 'expires=${invalid}' in cookies[0].unparsed
	}
	content := 'HTTP/1.1 200 OK\r\nSet-Cookie: a=b; expires=bogus; Expires=${valid}; Expires=Wed, 11 Nov 2009 23:00:00 GMT\r\nContent-Length: 0\r\n\r\n'
	response := parse_response(content)!
	cookies := response.cookies()
	assert cookies.len == 1
	assert cookies[0].expires.unix() == 1257980400
	assert cookies[0].raw_expires == 'Wed, 11 Nov 2009 23:00:00 GMT'
	assert 'expires=bogus' in cookies[0].unparsed
}

fn test_parse_response_with_weird_cookie() {
	// weird cookies test
	content_weird := 'HTTP/1.1 200 OK\r\nSet-Cookie: a=b; ; =; aa=; =bb; cc; ==\r\nContent-Length: 3\r\n\r\nFoo'
	mut xx := parse_response(content_weird)!
	weird_cookie := xx.cookies()
	assert weird_cookie[0].str() == 'a=b'
}

fn test_parse_response_with_gzip_content_encoding() {
	expected_body := '{"a": 1}'
	compressed_body := gzip.compress(expected_body.bytes())!
	content :=
		'HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Length: ${compressed_body.len}\r\n\r\n' +
			compressed_body.bytestr()
	resp := parse_response(content)!
	assert resp.body == expected_body
}

fn test_parse_response_with_deflate_content_encoding() {
	expected_body := '{"a": 1}'
	compressed_body := zlib.compress(expected_body.bytes())!
	content :=
		'HTTP/1.1 200 OK\r\nContent-Encoding: deflate\r\nContent-Length: ${compressed_body.len}\r\n\r\n' +
			compressed_body.bytestr()
	resp := parse_response(content)!
	assert resp.body == expected_body
}

fn test_parse_response_with_brotli_content_encoding() {
	if !brotli.is_available() {
		eprintln('skipping Brotli HTTP response test; libbrotli is not available')
		return
	}
	expected_body := '{"a": 1}'
	compressed_body := brotli.compress(expected_body.bytes(), mode: .text)!
	content :=
		'HTTP/1.1 200 OK\r\nContent-Encoding: br\r\nContent-Length: ${compressed_body.len}\r\n\r\n' +
			compressed_body.bytestr()
	resp := parse_response(content)!
	assert resp.body == expected_body
}

fn test_parse_response_with_chunked_and_gzip_content_encoding() {
	expected_body := '{"a": 1}'
	compressed_body := gzip.compress(expected_body.bytes())!
	chunked_body := '${compressed_body.len:x}\r\n' + compressed_body.bytestr() + '\r\n0\r\n\r\n'
	content := 'HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nContent-Encoding: gzip\r\n\r\n' +
		chunked_body
	resp := parse_response(content)!
	assert resp.body == expected_body
}

fn test_response_decompression_limit_and_preview() {
	body := 'x'.repeat(128)
	compressed := gzip.compress(body.bytes())!
	wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\n\r\n${compressed.bytestr()}'

	parse_response_with_options(wire, max_total_decompressed_bytes: 127) or {
		assert err is ResponseDecompressionLimitError, err.msg()
		return
	}
	assert false, 'expected the decoded-byte limit to be enforced'

	resp := parse_response_with_options(wire, max_total_decompressed_bytes: 256)!
	assert resp.body == body
	unlimited := parse_response_with_options(wire, max_total_decompressed_bytes: 0)!
	assert unlimited.body == body

	preview := parse_response_with_options(wire,
		max_total_decompressed_bytes: 256
		stop_copying_limit:           32
	)!
	assert preview.body == body[..32]
	copy_at_hard_limit := parse_response_with_options(wire,
		max_total_decompressed_bytes: body.len
		stop_copying_limit:           body.len
	)!
	assert copy_at_hard_limit.body == body
	too_large := body + 'x'
	too_large_compressed := gzip.compress(too_large.bytes())!
	too_large_wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\n\r\n${too_large_compressed.bytestr()}'
	parse_response_with_options(too_large_wire,
		max_total_decompressed_bytes: body.len
		stop_copying_limit:           body.len
	) or {
		assert err is ResponseDecompressionLimitError, err.msg()
		return
	}
	assert false, 'expected the hard budget to win when equal to the preview limit'

	// A preview aborts once its requested prefix is available. It does not read
	// the gzip trailer, so a checksum error after the preview is returned as that
	// decoded prefix rather than triggering the malformed-body raw fallback.
	mut corrupt := compressed.clone()
	corrupt[corrupt.len - 1] ^= 1
	corrupt_wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\n\r\n${corrupt.bytestr()}'
	corrupt_preview := parse_response_with_options(corrupt_wire,
		max_total_decompressed_bytes: 256
		stop_copying_limit:           32
	)!
	assert corrupt_preview.body == body[..32]
}

fn test_response_decompression_stage_budget_normalizes_large_limits() {
	chunk_size := i64(32768)
	int32_stage_limit := i64(max_i32) - chunk_size
	int64_stage_limit := max_i64 / 2
	int32_brotli_limit := i64(max_i32) - 1
	int64_brotli_limit := max_i64 / 2 + 1
	actual_stage_limit := i64(deflate.max_safe_inflate_output_len())
	if i64(max_int) == max_i64 {
		assert actual_stage_limit == int64_stage_limit
	} else {
		assert actual_stage_limit == int32_stage_limit
	}
	assert int32_stage_limit + 1 == i64(max_i32) - chunk_size + 1
	assert int64_stage_limit + 1 == (i64(1) << 62)
	assert response_decompression_stage_output_limit('gzip', i64(max_i32), int32_stage_limit) == int32_stage_limit
	assert response_decompression_stage_output_limit('br', i64(max_i32), int32_stage_limit) == int32_brotli_limit
	assert response_decompression_stage_output_limit('deflate', max_i64, int64_stage_limit) == int64_stage_limit
	assert response_decompression_stage_output_limit('br', max_i64, int64_stage_limit) == int64_brotli_limit

	for stage_limit in [int32_stage_limit, int64_stage_limit] {
		unlimited := response_decompression_stage_budget(0, 0, stage_limit)
		assert unlimited.output_limit_bytes == stage_limit
		assert !unlimited.total_budget

		negative_unlimited := response_decompression_stage_budget(-1, 99, stage_limit)
		assert negative_unlimited.output_limit_bytes == stage_limit
		assert !negative_unlimited.total_budget

		large_configured_limit := response_decompression_stage_budget(max_i64, 0, stage_limit)
		assert large_configured_limit.output_limit_bytes == stage_limit
		assert !large_configured_limit.total_budget

		exact_total_budget := response_decompression_stage_budget(stage_limit, 0, stage_limit)
		assert exact_total_budget.output_limit_bytes == stage_limit
		assert exact_total_budget.total_budget

		remaining_total_budget := response_decompression_stage_budget(stage_limit + 100, 100,
			stage_limit)
		assert remaining_total_budget.output_limit_bytes == stage_limit
		assert remaining_total_budget.total_budget
	}

	too_large_request := response_decompression_stage_budget(10, 11, int32_stage_limit)
	assert too_large_request.output_limit_bytes == -1
	assert too_large_request.total_budget

	for total_limit in [i64(0), i64(-1), max_i64] {
		encoded := gzip.compress('small body'.bytes())!
		wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\n\r\n${encoded.bytestr()}'
		resp := parse_response_with_options(wire,
			max_total_decompressed_bytes: total_limit
			stop_copying_limit:           max_i64 - 1
		)!
		assert resp.body == 'small body'
	}

	stage_error := response_decompression_limit_error(0, int32_stage_limit)
	assert stage_error.code() == response_decompression_limit_error_code
	assert is_no_need_retry_error(stage_error.code())
	assert stage_error.max_stage_decompressed_bytes == int32_stage_limit
	assert stage_error.msg().contains('per-stage representation limit')
}

fn test_response_decompression_limit_applies_to_all_codecs() {
	body := 'codec budget'.repeat(20)
	for encoding in ['gzip', 'deflate', 'br'] {
		if encoding == 'br' && !brotli.is_available() {
			eprintln('skipping Brotli response limit test; libbrotli is not available')
			continue
		}
		compressed := match encoding {
			'gzip' { gzip.compress(body.bytes())! }
			'deflate' { zlib.compress(body.bytes())! }
			else { brotli.compress(body.bytes())! }
		}
		wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: ${encoding}\r\n\r\n${compressed.bytestr()}'
		parse_response_with_options(wire, max_total_decompressed_bytes: 64) or {
			assert err is ResponseDecompressionLimitError, 'encoding=${encoding}: ${err.msg()}'
			continue
		}
		assert false, 'expected ${encoding} output to exceed the limit'
	}
}

fn test_response_decompression_budget_is_cumulative_across_encodings() {
	final_body := 'layer'.repeat(12)
	inner := gzip.compress(final_body.bytes())!
	outer := gzip.compress(inner)!
	wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: gzip, gzip\r\n\r\n${outer.bytestr()}'
	budget := i64(inner.len + final_body.len)
	resp := parse_response_with_options(wire, max_total_decompressed_bytes: budget)!
	assert resp.body == final_body

	parse_response_with_options(wire, max_total_decompressed_bytes: budget - 1) or {
		assert err is ResponseDecompressionLimitError, err.msg()
		return
	}
	assert false, 'expected the cumulative decoded-byte budget to be enforced'
}

fn test_response_decompression_layer_limit() {
	mut encoded := 'x'.bytes()
	for _ in 0 .. max_content_encoding_layers {
		encoded = gzip.compress(encoded)!
	}
	encodings := 'gzip, '.repeat(max_content_encoding_layers - 1) + 'gzip'
	mut wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: ${encodings}\r\n\r\n${encoded.bytestr()}'
	assert parse_response(wire)!.body == 'x'

	encoded = gzip.compress(encoded)!
	too_many := 'gzip, ${encodings}'
	wire = 'HTTP/1.1 200 OK\r\nContent-Encoding: ${too_many}\r\n\r\n${encoded.bytestr()}'
	parse_response(wire) or {
		assert err is ResponseContentEncodingLayerLimitError, err.msg()
		assert err.code() == response_content_encoding_layer_limit_error_code
		assert is_no_need_retry_error(err.code())
		assert err.msg().contains('more than ${max_content_encoding_layers}')
		return
	}
	assert false, 'expected too many Content-Encoding layers to be rejected'
}

fn test_response_default_decompression_limit_boundary() {
	assert default_max_total_decompressed_bytes == 16 * 1024 * 1024
	boundary := 'a'.repeat(int(default_max_total_decompressed_bytes))
	compressed := gzip.compress(boundary.bytes())!
	wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\n\r\n${compressed.bytestr()}'
	resp := parse_response(wire)!
	assert resp.body.len == boundary.len
	assert resp.body == boundary

	too_large := boundary + 'a'
	compressed_large := gzip.compress(too_large.bytes())!
	too_large_wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\n\r\n${compressed_large.bytestr()}'
	parse_response(too_large_wire) or {
		assert err is ResponseDecompressionLimitError, err.msg()
		return
	}
	assert false, 'expected the default decompression limit to reject a larger body'
}

fn test_vschannel_error_message_formats_non_wsa_errors() {
	assert vschannel_error_message(-2146893052) == '0x80090304'
	assert vschannel_error_message(42) == '42'
}

fn test_vschannel_error_message_formats_windows_wsa_errors() {
	$if windows {
		assert vschannel_error_message(11001) == '(11001) wsahost_not_found'
	}
}

fn test_vschannel_request_error_normalizes_connect_failures() {
	err := vschannel_request_error(vschannel_sec_e_internal_error)
	assert err.msg() == vschannel_connect_failed_msg
	assert err.code() == vschannel_sec_e_internal_error
}

fn test_vschannel_request_error_keeps_other_codes() {
	err := vschannel_request_error(42)
	assert err.msg() == 'http: vschannel request failed: 42'
	assert err.code() == 42
}

fn test_vschannel_parse_response_normalizes_connect_failure_output() {
	vschannel_parse_response('Error 10057 sending data to server (1)\nError performing handshake',
		0) or {
		assert err.msg() == vschannel_connect_failed_msg
		assert err.code() == 0
		return
	}
	assert false
}

fn test_proxy_response_parsing_preserves_encoded_representations() {
	for encoding in ['gzip', 'deflate', 'br'] {
		compressed := match encoding {
			'gzip' { gzip.compress('encoded upstream body'.bytes())! }
			'deflate' { zlib.compress('encoded upstream body'.bytes())! }
			else { brotli.compress('encoded upstream body'.bytes())! }
		}
		for chunked in [false, true] {
			framing := if chunked {
				'Transfer-Encoding: chunked'
			} else {
				'Content-Length: ${compressed.len}'
			}
			body := if chunked {
				'${compressed.len:x}\r\n${compressed.bytestr()}\r\n0\r\n\r\n'
			} else {
				compressed.bytestr()
			}
			wire := 'HTTP/1.1 200 OK\r\nContent-Encoding: ${encoding}\r\n${framing}\r\n\r\n${body}'
			assert parse_response(wire)!.body == 'encoded upstream body'
			assert parse_received_response(wire, ReceivedResponseInfo{ raw_body: true })!.body.bytes() == compressed
			assert vschannel_parse_response(wire, 0, raw_body: true)!.body.bytes() == compressed
		}
	}
}
