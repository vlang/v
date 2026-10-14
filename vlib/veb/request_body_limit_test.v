module veb

import io

struct BodyFragmentReader {
	bytes       []u8
	step        int = 2
	zero_at_end bool
mut:
	pos int
}

fn (mut source BodyFragmentReader) read(mut buf []u8) !int {
	if source.pos == source.bytes.len {
		if source.zero_at_end { return 0 }
		return io.Eof{}
	}
	mut count := source.step
	if count > buf.len { count = buf.len }
	if count > source.bytes.len - source.pos { count = source.bytes.len - source.pos }
	for i in 0 .. count { buf[i] = source.bytes[source.pos + i] }
	source.pos += count
	return count
}

fn parse_limited_body(framing string, body string, limit int, zero_at_end bool) !string {
	mut source := BodyFragmentReader{
		bytes:       ('PUT /upload HTTP/1.1\r\nHost: localhost\r\n' + framing + '\r\n\r\n' + body).bytes()
		zero_at_end: zero_at_end
	}
	mut reader := io.new_buffered_reader(reader: source, cap: 7)
	return read_request_from_buffered_reader(mut reader, limit)!.data
}

fn test_content_length_requires_complete_decimal_and_int_range() {
	for value in ['', '-1', '+1', '1x', '0x10', '1, 1', '9223372036854775808', '18446744073709551616'] {
		if _ := parse_limited_body('Content-Length: ' + value, '', 3, false) {
			assert false, 'accepted invalid Content-Length ' + value
		} else {
			assert request_read_error_response(err).status_code == 400
		}
	}
	assert parse_request_content_length('0003')! == 3
	assert parse_request_content_length(max_int.str())! == max_int
}

fn test_content_length_limit_before_body_read_and_exact_fragmented_boundary() {
	assert parse_limited_body('Content-Length: 3', 'abc', 3, false)! == 'abc'
	assert parse_limited_body('Content-Length: 3', 'abc', 0, false)! == 'abc'
	if _ := parse_limited_body('Content-Length: 4', '', 3, false) {
		assert false
	} else {
		assert err is RequestBodyLimitError
		assert request_read_error_response(err).status_code == 413
		assert (request_read_error_response(err).header.get(.connection) or { '' }) == 'close'
	}
}

fn test_chunked_body_cumulative_limit_and_fragmented_trailers() {
	chunks := '2;tag=ok\r\nab\r\n1\r\nc\r\n0\r\nX-Test: yes\r\n\r\n'
	assert parse_limited_body('Transfer-Encoding: chunked', chunks, 3, false)! == 'abc'
	assert parse_limited_body('Transfer-Encoding: chunked', chunks, 0, false)! == 'abc'
	if _ := parse_limited_body('Transfer-Encoding: chunked', '2\r\nab\r\n2\r\n', 3, false) {
		assert false
	} else {
		assert err is RequestBodyLimitError
		assert request_read_error_response(err).status_code == 413
	}
}

fn test_chunk_size_rejects_prefixes_junk_and_overflow() {
	for value in ['+1', '-1', '1x', '0x1', '10000000000000000', '8000000000000000'] {
		if _ := parse_limited_body('Transfer-Encoding: chunked', value + '\r\n', 0, false) {
			assert false, 'accepted invalid chunk ' + value
		} else {
			assert request_read_error_response(err).status_code == 400
		}
	}
}

fn test_truncated_and_zero_progress_body_read_returns_error() {
	for zero_at_end in [false, true] {
		for fixture in ['Content-Length: 3\n:a', 'Transfer-Encoding: chunked\n:2\r\na',
			'Transfer-Encoding: chunked\n:1\r\na\r', 'Transfer-Encoding: chunked\n:1\r\naXX'] {
			parts := fixture.split('\n:')
			if _ := parse_limited_body(parts[0], parts[1], 3, zero_at_end) {
				assert false, 'accepted truncated/malformed body'
			} else {
				assert request_read_error_response(err).status_code == 400
			}
		}
	}
}

fn test_conflicting_lengths_cannot_hide_an_oversized_body() {
	for framing in ['Content-Length: 1\r\nContent-Length: 4', 'Content-Length: 1\r\nContent-Length: 4x',
		'Content-Length: 1\r\nTransfer-Encoding: chunked', 'Content-Length: 1\r\nTransfer-Encoding: gzip',
		'Content-Length: 1\r\nTransfer-Encoding: gzip\r\nTransfer-Encoding: chunked',
		'Transfer-Encoding: gzip\r\nContent-Length: 0',
		'Transfer-Encoding: gzip\r\nTransfer-Encoding: chunked\r\nContent-Length: 1'] {
		if _ := parse_limited_body(framing, 'a', 3, false) {
			assert false, 'accepted ambiguous request framing'
		} else {
			assert request_read_error_response(err).status_code == 400
		}
	}
	assert parse_limited_body('Content-Length: 3\r\nContent-Length: 3', 'abc', 3, false)! == 'abc'
}
