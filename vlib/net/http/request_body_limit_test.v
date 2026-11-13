module http

import io

struct RequestBodyLimitTestReader {
	text string
mut:
	position int
}

fn (mut source RequestBodyLimitTestReader) read(mut buffer []u8) !int {
	if source.position >= source.text.len {
		return io.Eof{}
	}
	n := copy(mut buffer, source.text[source.position..].bytes())
	source.position += n
	return n
}

fn parse_request_with_test_limit(raw string, limit int) !Request {
	mut reader := io.new_buffered_reader(
		reader: &RequestBodyLimitTestReader{
			text: raw
		}
	)
	return parse_request_with_body_limit(mut reader, limit)
}

fn test_request_body_limit_accepts_exact_boundary_and_zero_means_unlimited() {
	request := parse_request_with_test_limit('POST / HTTP/1.1\r\nContent-Length: 3\r\n\r\nabc', 3)!
	assert request.data == 'abc'
	unlimited := parse_request_with_test_limit('POST / HTTP/1.1\r\nContent-Length: 4\r\n\r\nbody', 0)!
	assert unlimited.data == 'body'
}

fn test_request_body_limit_rejects_before_reading_body() {
	if _ := parse_request_with_test_limit('POST / HTTP/1.1\r\nContent-Length: 4\r\n\r\n', 3) {
		assert false, 'accepted Content-Length above the configured limit'
	} else {
		assert err is RequestBodyLimitError
	}
}

fn test_public_parse_request_has_a_safe_default_limit() {
	mut reader := io.new_buffered_reader(
		reader: &RequestBodyLimitTestReader{
			text: 'POST / HTTP/1.1\r\nContent-Length: ${default_max_http1_request_body_size + 1}\r\n\r\n'
		}
	)
	if _ := parse_request(mut reader) {
		assert false, 'public parse_request accepted a body above its default limit'
	} else {
		assert err is RequestBodyLimitError
	}
}

fn test_request_head_str_enforces_total_size_limit() {
	prefix := 'GET / HTTP/1.1\r\nX: '
	suffix := '\r\n\r\n'
	value_len := default_max_http1_request_head_size - prefix.len - suffix.len
	within_limit := prefix + 'a'.repeat(value_len) + suffix
	assert within_limit.len == default_max_http1_request_head_size
	parse_request_head_str(within_limit) or { panic('accepted boundary request head: ${err}') }

	over_limit := prefix + 'a'.repeat(value_len + 1) + suffix
	if _ := parse_request_head_str(over_limit) {
		assert false, 'accepted request head above the configured size limit'
	} else {
		assert err is RequestHeaderLimitError
	}
}

fn test_request_body_limit_rejects_invalid_content_lengths() {
	invalid_values := ['', '-1', '+1', '1x', '${max_int}0']
	for value in invalid_values {
		if _ := parse_request_with_test_limit('POST / HTTP/1.1\r\nContent-Length: ${value}\r\n\r\n', 3) {
			assert false, 'accepted invalid Content-Length `${value}`'
		} else {
			assert err is RequestFramingError
		}
	}
	with_ows := parse_request_with_test_limit('POST / HTTP/1.1\r\nContent-Length: 3 \t\r\n\r\nabc', 3)!
	assert with_ows.data == 'abc'
}

fn test_request_body_limit_checks_duplicate_content_lengths() {
	identical := parse_request_with_test_limit('POST / HTTP/1.1\r\nContent-Length: 3\r\nContent-Length: 03\r\n\r\nabc', 3)!
	assert identical.data == 'abc'
	if _ := parse_request_with_test_limit('POST / HTTP/1.1\r\nContent-Length: 3\r\nContent-Length: 4\r\n\r\n', 4) {
		assert false, 'accepted conflicting Content-Length values'
	} else {
		assert err is RequestFramingError
	}
}

fn test_request_body_limit_rejects_unsupported_transfer_encoding() {
	for headers in ['Transfer-Encoding: chunked', 'Transfer-Encoding: gzip',
		'Content-Length: 1\r\nTransfer-Encoding: chunked'] {
		if _ := parse_request_with_test_limit('POST / HTTP/1.1\r\n${headers}\r\n\r\n', 3) {
			assert false, 'accepted unsupported request framing: ${headers}'
		} else {
			assert err is RequestFramingError
		}
	}
}

fn test_request_body_limit_rejects_negative_limit() {
	if _ := parse_request_with_test_limit('GET / HTTP/1.1\r\n\r\n', -1) {
		assert false, 'accepted a negative request body limit'
	}
}
