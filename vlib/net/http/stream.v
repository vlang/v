module http

import net

// StreamFn writes the body of a streamed response: see `Response.stream`. An
// error it returns ends the stream; the connection is closed either way.
pub type StreamFn = fn (mut writer StreamWriter) !

// StreamWriter writes a streamed response's body to its connection.
pub struct StreamWriter {
mut:
	conn &net.TcpConn
}

// write sends `data` as it is.
pub fn (mut w StreamWriter) write(data []u8) !int {
	return w.conn.write(data)
}

// write_string sends `text` as it is.
pub fn (mut w StreamWriter) write_string(text string) !int {
	return w.conn.write_string(text)
}

// write_event sends one server-sent event, framed by `SSEEvent.encode`.
pub fn (mut w StreamWriter) write_event(event SSEEvent) ! {
	w.conn.write_string(event.encode())!
}
