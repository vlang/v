module websocket

import net
import net.http

// socket_read reads from socket into the provided buffer
fn (mut ws Client) socket_read(mut buffer []u8) !int {
	return ws.socket_read_ptr(buffer.data, buffer.len)
}

// socket_read_ptr reads ahead, retaining unused bytes for the next frame field.
// Like the frame parser, the buffer belongs to the connection's single reader.
fn (mut ws Client) socket_read_ptr(buf_ptr &u8, len int) !int {
	if ws.get_state() in [.closed, .closing] || ws.conn.sock.handle <= 1 {
		return error('socket_read_ptr: trying to read a closed socket')
	}
	if len <= 0 {
		return 0
	}
	if ws.read_start == ws.read_end {
		ws.read_start = 0
		ws.read_end = 0
		// Large payloads can go directly into their final allocation.
		if len >= ws.read_buffer.len {
			return ws.socket_read_unbuffered(buf_ptr, len)
		}
		ws.read_end = ws.socket_read_unbuffered(&ws.read_buffer[0], ws.read_buffer.len)!
		if ws.read_end <= 0 {
			ws.read_end = 0
			return 0
		}
	}
	available := ws.read_end - ws.read_start
	count := if available < len { available } else { len }
	unsafe { vmemcpy(buf_ptr, &ws.read_buffer[ws.read_start], count) }
	ws.read_start += count
	return count
}

fn (mut ws Client) socket_read_unbuffered(buf_ptr &u8, len int) !int {
	if ws.is_ssl {
		return ws.ssl_conn.socket_read_into_ptr(buf_ptr, len)
	}
	return ws.conn.read_ptr(buf_ptr, len)
}

// socket_write writes the provided byte array to the socket
fn (mut ws Client) socket_write(bytes []u8) !int {
	// Serialize complete frames/batches, including control frames from listen().
	lock ws.write_lock {
		if ws.get_state() == .closed || ws.conn.sock.handle <= 1 {
			return error('socket_write: trying to write on a closed socket')
		}
		if ws.is_ssl {
			return ws.ssl_conn.write(bytes)
		}
		// TcpConn.write already completes short writes. A timeout may follow a
		// partial send: replaying the whole buffer would corrupt frame boundaries.
		return ws.conn.write(bytes)
	}
}

// shutdown_socket shuts down the socket properly when connection is closed
fn (mut ws Client) shutdown_socket() ! {
	ws.debug_log('shutting down socket')
	if ws.is_ssl {
		ws.ssl_conn.shutdown()!
	} else {
		ws.conn.close()!
	}
}

// dial_socket connects tcp socket and initializes default configurations
fn (mut ws Client) dial_socket() !&net.TcpConn {
	tcp_address := '${ws.uri.hostname}:${ws.uri.port}'
	mut t := if ws.proxy_url == '' {
		net.dial_tcp(tcp_address)!
	} else {
		http.dial_tcp_via_proxy(ws.proxy_url, tcp_address)!
	}
	optval := int(1)
	t.sock.set_option_int(.keep_alive, optval)!
	t.set_read_timeout(ws.read_timeout)
	t.set_write_timeout(ws.write_timeout)
	if ws.is_ssl {
		ws.ssl_conn.connect(mut t, ws.uri.hostname) or {
			// The TcpConn is not the client's yet, so nothing else can close it: a failed TLS
			// handshake would otherwise leak the connected socket, and the peer's half of it.
			t.close() or {}
			return err
		}
	}
	return t
}
