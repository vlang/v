module http

import strings

// SSEEvent is a single Server-Sent Events message, as defined by the
// event-stream format (https://html.spec.whatwg.org/multipage/server-sent-events.html).
// It is the shared SSE encoder for vlib: `veb.sse`, veb live-reload, and `mcp`
// stream responses all frame events through it, so every SSE producer in vlib
// emits byte-identical output for the same input.
pub struct SSEEvent {
pub mut:
	id    string
	event string
	data  string
	retry int
}

// encode serializes the event to the SSE wire format.
// Multi-line `data` is split into one `data:` line per line, as required by
// the specification: a bare newline inside `data` would otherwise terminate
// the field early and corrupt the event framing. CRLF and lone CR line endings
// are normalized to LF before splitting. `id` and `event` must be single-line,
// so line breaks are stripped from them to avoid injecting extra fields.
pub fn (e SSEEvent) encode() string {
	mut sb := strings.new_builder(128)
	e.encode_into(mut sb)
	return sb.str()
}

// encode_into appends the wire format of the event to `sb`, reusing its buffer.
// Prefer it over `encode` when serializing several events into one payload,
// so only a single allocation backs the whole stream.
pub fn (e SSEEvent) encode_into(mut sb strings.Builder) {
	if e.id != '' {
		sb.write_string('id: ')
		sb.write_string(e.id.replace('\r', '').replace('\n', ''))
		sb.write_string('\n')
	}
	if e.event != '' {
		sb.write_string('event: ')
		sb.write_string(e.event.replace('\r', '').replace('\n', ''))
		sb.write_string('\n')
	}
	if e.data != '' {
		normalized := e.data.replace('\r\n', '\n').replace('\r', '\n')
		for line in normalized.split('\n') {
			sb.write_string('data: ')
			sb.write_string(line)
			sb.write_string('\n')
		}
	}
	if e.retry != 0 {
		sb.write_string('retry: ')
		sb.write_string(e.retry.str())
		sb.write_string('\n')
	}
	sb.write_string('\n')
}
