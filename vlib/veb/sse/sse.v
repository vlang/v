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
	ctx.res.header.set(.connection, 'keep-alive')
	ctx.res.header.set(.cache_control, 'no-cache')
	ctx.send_response_to_client('text/event-stream', '')

	return &SSEConnection{
		conn: ctx.conn
	}
}

// send_message sends a single message to the http client that listens for SSE.
// It does not close the connection, so you can use it many times in a loop.
pub fn (mut sse SSEConnection) send_message(message SSEMessage) ! {
	mut sb := strings.new_builder(512)
	if message.id != '' {
		sb.write_string('id: ${message.id}\n')
	}
	if message.event != '' {
		sb.write_string('event: ${message.event}\n')
	}
	if message.data != '' {
		sb.write_string('data: ${message.data}\n')
	}
	if message.retry != 0 {
		sb.write_string('retry: ${message.retry}\n')
	}
	sb.write_string('\n')
	sse.conn.write(sb)!
}

// send a 'close' event and close the tcp connection.
pub fn (mut sse SSEConnection) close() {
	sse.send_message(event: 'close', data: 'Closing the connection', retry: -1) or {}
	sse.conn.close() or {}
}
