module sse

import net
import time
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

[heap]
pub struct SSEConnection {
pub mut:
	headers       map[string]string
	conn          &net.TcpConn
	write_timeout time.Duration = 600 * time.second
}

pub struct SSEMessage {
	id    string
	event string
	data  string
	retry int
}

pub fn new_connection(conn &net.TcpConn) &SSEConnection {
	return &SSEConnection{
		conn: unsafe { conn }
	}
}

// sse_start is used to send the start of a Server Side Event response.
pub fn (mut sse SSEConnection) start() ? {
	sse.conn.set_write_timeout(sse.write_timeout)
	mut start_sb := strings.new_builder(512)
	start_sb.write_string('HTTP/1.1 200')
	start_sb.write_string('\r\nConnection: keep-alive')
	start_sb.write_string('\r\nCache-Control: no-cache')
	start_sb.write_string('\r\nContent-Type: text/event-stream')
	for k, v in sse.headers {
		start_sb.write_string('\r\n$k: $v')
	}
	start_sb.write_string('\r\n')
	sse.conn.write(start_sb) or { return error('could not start sse response') }
}

// send_message sends a single message to the http client that listens for SSE.
// It does not close the connection, so you can use it many times in a loop.
pub fn (mut sse SSEConnection) send_message(message SSEMessage) ? {
	mut sb := strings.new_builder(512)
	if message.id != '' {
		sb.write_string('id: $message.id\n')
	}
	if message.event != '' {
		sb.write_string('event: $message.event\n')
	}
	if message.data != '' {
		sb.write_string('data: $message.data\n')
	}
	if message.retry != 0 {
		sb.write_string('retry: $message.retry\n')
	}
	sb.write_string('\n')
	sse.conn.write(sb)?
}
