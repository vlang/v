module veb

import io
import net.http
import net.ssl
import os
import time

// TlsConn is an HTTPS connection taken over by a handler. Serialize all calls
// on the connection, including reads, writes and close.
@[heap]
pub struct TlsConn {
mut:
	conn   &ssl.SSLConn
	reader &io.BufferedReader
	closed bool
}

// read reads decrypted bytes, starting with any bytes buffered by the HTTP parser.
pub fn (mut conn TlsConn) read(mut buf []u8) !int {
	if conn.closed {
		return io.Eof{}
	}
	return conn.reader.read(mut buf)
}

// read_with_timeout reads HTTP lookahead first, then reads from TLS with a
// temporary timeout. It preserves timeout errors without marking the buffered
// reader as EOF. As with read, calls on the connection must be serialized.
pub fn (mut conn TlsConn) read_with_timeout(mut buf []u8, timeout time.Duration) !int {
	if conn.closed {
		return io.Eof{}
	}
	if conn.reader.buffered() > 0 {
		return conn.reader.read(mut buf)
	}
	previous := conn.conn.read_timeout()
	previous_duration := conn.conn.duration
	conn.conn.set_read_timeout(timeout)
	defer {
		conn.conn.set_read_timeout(previous)
		conn.conn.duration = previous_duration
	}
	return conn.conn.read(mut buf)
}

// write writes bytes to the client over TLS.
pub fn (mut conn TlsConn) write(buf []u8) !int {
	if conn.closed {
		return error('TLS connection is closed')
	}
	return conn.conn.write(buf)
}

// write_string writes a string to the client over TLS.
pub fn (mut conn TlsConn) write_string(s string) !int {
	return conn.write(s.bytes())
}

// close releases the TLS session, socket and read buffer.
pub fn (mut conn TlsConn) close() ! {
	if conn.closed {
		return
	}
	conn.closed = true
	conn.reader.free()
	conn.conn.close()!
}

// Shared by copies of the request context. Only the request's dispatch can claim it.
struct TlsTakeover {
	conn   &ssl.SSLConn
	reader &io.BufferedReader
mut:
	available bool = true
	claimed   bool
}

// tls_socket_handle reports the underlying socket for shutdown notification.
// The caller must retain its own handle if it needs it after request dispatch.
pub fn (ctx &Context) tls_socket_handle() ?int {
	takeover := ctx.tls_takeover or { return none }
	if !takeover.available {
		return none
	}
	return takeover.conn.handle
}

// takeover_tls_conn transfers the active HTTPS connection to the handler.
// Call it during HTTPS request handling, before spawning any work that uses the
// connection. On success the handler must write the wire response, return
// no_result(), and arrange to close the connection. An error leaves ownership
// with the current owner.
pub fn (mut ctx Context) takeover_tls_conn() !&TlsConn {
	mut takeover := ctx.tls_takeover or { return error('no HTTPS connection to take over') }
	if !takeover.available {
		return error('HTTPS connection takeover is no longer available')
	}
	takeover.available = false
	takeover.claimed = true
	ctx.takeover_mode = .manual
	return &TlsConn{
		conn:   takeover.conn
		reader: takeover.reader
	}
}

fn handle_ssl_connection[A, X](mut ssl_conn ssl.SSLConn, params &SslRequestParams) {
	mut reader := io.new_buffered_reader(
		reader: &ssl_conn
		cap:    params.max_request_buffer_size
	)
	mut taken_over := false
	defer {
		if !taken_over {
			reader.free()
			ssl_conn.shutdown() or {}
		}
	}
	for {
		req := read_request_from_buffered_reader_with_timeout(mut reader, params.max_request_body_size,
			mut ssl_conn, params.read_header_timeout) or {
			if err !is io.Eof {
				write_ssl_response(mut ssl_conn, request_read_error_response(err)) or {}
			}
			return
		}
		mut takeover := &TlsTakeover{
			conn:   &ssl_conn
			reader: reader
		}
		completed_context := handle_ssl_request[A, X](req, params, takeover)
		takeover.available = false
		if takeover.claimed {
			taken_over = true
			return
		}
		ctx := completed_context or {
			write_ssl_response(mut ssl_conn, http_400) or {}
			return
		}
		if ctx.takeover_mode != .none {
			eprintln('[veb] use Context.takeover_tls_conn() to take over an HTTPS connection')
		}
		write_ssl_context_response(mut ssl_conn, ctx) or {
			eprintln('[veb] error sending HTTPS response: ${err}')
			return
		}
		if ctx.takeover_mode != .none
			|| should_close_connection(ctx.req, ctx.res, ctx.client_wants_to_close) {
			return
		}
	}
}

fn write_ssl_context_response(mut ssl_conn ssl.SSLConn, completed_context &Context) ! {
	if !completed_context.done && completed_context.return_type == .normal {
		return error('context did not send a response')
	}
	match completed_context.return_type {
		.normal {
			write_ssl_response(mut ssl_conn, completed_context.res)!
		}
		.file {
			write_ssl_response(mut ssl_conn, completed_context.res)!
			if completed_context.return_file == '' {
				return error('missing file response path')
			}
			mut file := os.open(completed_context.return_file)!
			defer {
				file.close()
			}
			mut buf := []u8{len: max_read}
			for {
				n := file.read(mut buf) or {
					if err is io.Eof {
						break
					}
					return err
				}
				if n <= 0 {
					break
				}
				ssl_conn.write(buf[..n])!
			}
		}
	}
}

fn write_ssl_response(mut ssl_conn ssl.SSLConn, resp http.Response) ! {
	ssl_conn.write(resp.bytes())!
}
