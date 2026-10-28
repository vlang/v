import strings

// Pure unit tests for the shared `http.SSEEvent` encoder. No network involved.
// Note: `strings` is already available through `net.http`, importing it again
// is an error.
import net.http

fn test_sse_event_single_line_data() {
	e := http.SSEEvent{
		data: 'ping'
	}
	assert e.encode() == 'data: ping\n\n'
}

fn test_sse_event_all_fields_single_line() {
	e := http.SSEEvent{
		id:    '1'
		event: 'update'
		data:  'payload'
		retry: 3000
	}
	assert e.encode() == 'id: 1\nevent: update\ndata: payload\nretry: 3000\n\n'
}

fn test_sse_event_multiline_data_splits_per_line() {
	e := http.SSEEvent{
		data: 'line1\nline2\nline3'
	}
	assert e.encode() == 'data: line1\ndata: line2\ndata: line3\n\n'
}

fn test_sse_event_crlf_and_cr_are_normalized() {
	e := http.SSEEvent{
		data: 'a\r\nb\rc'
	}
	assert e.encode() == 'data: a\ndata: b\ndata: c\n\n'
}

fn test_sse_event_empty_message_is_blank_line() {
	e := http.SSEEvent{}
	assert e.encode() == '\n'
}

fn test_sse_event_id_and_event_newlines_are_stripped() {
	e := http.SSEEvent{
		id:    'a\nb'
		event: 'x\ry'
		data:  'ok'
	}
	assert e.encode() == 'id: ab\nevent: xy\ndata: ok\n\n'
}

fn test_sse_event_encode_into_reuses_one_buffer() {
	mut sb := strings.new_builder(128)
	http.SSEEvent{
		data: 'a'
	}.encode_into(mut sb)
	http.SSEEvent{
		event: 'change'
		data:  'css'
	}.encode_into(mut sb)
	assert sb.str() == 'data: a\n\nevent: change\ndata: css\n\n'
}
