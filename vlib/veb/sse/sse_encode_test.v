import veb.sse

fn test_encode_single_line_is_unchanged() {
	m := sse.SSEMessage{
		data: 'ping'
	}
	assert m.encode() == 'data: ping\n\n'
}

fn test_encode_all_fields_single_line() {
	m := sse.SSEMessage{
		id:    '1'
		event: 'update'
		data:  'payload'
		retry: 3000
	}
	assert m.encode() == 'id: 1\nevent: update\ndata: payload\nretry: 3000\n\n'
}

fn test_encode_multiline_data_splits_per_line() {
	m := sse.SSEMessage{
		data: 'line1\nline2\nline3'
	}
	assert m.encode() == 'data: line1\ndata: line2\ndata: line3\n\n'
}

fn test_encode_crlf_and_cr_are_normalized() {
	m := sse.SSEMessage{
		data: 'a\r\nb\rc'
	}
	assert m.encode() == 'data: a\ndata: b\ndata: c\n\n'
}

fn test_encode_empty_message_is_blank_line() {
	m := sse.SSEMessage{}
	assert m.encode() == '\n'
}

fn test_encode_id_and_event_newlines_are_stripped() {
	m := sse.SSEMessage{
		id:    'a\nb'
		event: 'x\ry'
		data:  'ok'
	}
	assert m.encode() == 'id: ab\nevent: xy\ndata: ok\n\n'
}
