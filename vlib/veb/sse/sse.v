module sse

import veb
import net
import strings

// This module implements the server side of `Server Sent Events`.
// See https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format
// as well as https://html.spec.whatwg.org/multipage/server-sent-events.html#server-sent-events
// for detailed description of the protocol, and a simple web browser client example.
//
// > Event stream format
// > The event stream is a simple stream of text data which must be encoded using UTF-8.
// > Messages in the event stream are separated by a pair of newline characters.
// > A colon as the first character of a line is in essence a comment, and is ignored.
// > Note: The comment line can be used to prevent connections from timing out;
// > a server can send a comment periodically to keep the connection alive.
// >
// > Each message consists of one or more lines of text listing the fields for that message.
// > Each field is represented by the field name, followed by a colon, followed by the text
// > data for that field's value.

@[params]
pub struct SSEMessage {
pub mut:
	id    string
	event string
	data  string
	retry int
}

@[heap]
pub struct SSEConnection {
pub mut:
	conn &net.TcpConn @[required]
}

// start an SSE connection
pub fn start_connection(mut ctx veb.Context) &SSEConnection {
	if ctx.conn == unsafe { nil } {
		eprintln('[veb.sse] WARNING: SSE requires a direct TCP connection (ctx.conn) which is not available. Use `ctx.takeover_conn()` before starting an SSE connection.')
		return &SSEConnection{
			conn: unsafe { nil }
		}
	}
	// Build and send HTTP response headers directly.
	// SSE responses must NOT include Content-Length since data is streamed.
	mut sb := strings.new_builder(256)
	sb.write_string('HTTP/1.1 200 OK\r\n')
	sb.write_string('Content-Type: text/event-stream\r\n')
	sb.write_string('Connection: keep-alive\r\n')
	sb.write_string('Cache-Control: no-cache\r\n')
	sb.write_string('Server: veb\r\n')
	sb.write_string('\r\n')
	ctx.conn.write(sb) or {}

	return &SSEConnection{
		conn: ctx.conn
	}
}

// encode serializes the message to the SSE wire format.
// Multi-line `data` is split into one `data:` line per line, as required by
// the SSE specification: a bare newline inside `data` would otherwise terminate
// the field early and corrupt the event framing. CRLF and lone CR line endings
// are normalized to LF before splitting.
pub fn (m SSEMessage) encode() string {
	mut sb := strings.new_builder(512)
	if m.id != '' {
		sb.write_string('id: ')
		// `id` and `event` must be single-line; strip line breaks to avoid
		// injecting extra fields or terminating the event early.
		sb.write_string(m.id.replace('\r', '').replace('\n', ''))
		sb.write_string('\n')
	}
	if m.event != '' {
		sb.write_string('event: ')
		sb.write_string(m.event.replace('\r', '').replace('\n', ''))
		sb.write_string('\n')
	}
	if m.data != '' {
		normalized := m.data.replace('\r\n', '\n').replace('\r', '\n')
		for line in normalized.split('\n') {
			sb.write_string('data: ')
			sb.write_string(line)
			sb.write_string('\n')
		}
	}
	if m.retry != 0 {
		sb.write_string('retry: ${m.retry}\n')
	}
	sb.write_string('\n')
	return sb.str()
}

// send_message sends a single message to the http client that listens for SSE.
// It does not close the connection, so you can use it many times in a loop.
pub fn (mut sse SSEConnection) send_message(message SSEMessage) ! {
	if sse.conn == unsafe { nil } {
		return error('SSE connection is not available (no TCP connection)')
	}
	sse.conn.write(message.encode().bytes())!
}

// send a 'close' event and close the tcp connection.
pub fn (mut sse SSEConnection) close() {
	if sse.conn == unsafe { nil } {
		return
	}
	sse.send_message(event: 'close', data: 'Closing the connection', retry: -1) or {}
	sse.conn.close() or {}
}
