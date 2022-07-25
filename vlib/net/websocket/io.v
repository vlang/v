module websocket

import net

// socket_read reads from socket into the provided buffer
fn (mut ws Client) socket_read(mut buffer []u8) ?int {
	lock  {
		if ws.state in [.closed, .closing] || ws.conn.sock.handle <= 1 {
			return error('socket_read: trying to read a closed socket')
		}
		if ws.is_ssl {
			r := ws.ssl_conn.read(mut buffer)?
			return r
		} else {
			r := ws.conn.read(mut buffer)?
			return r
		}
	}
	return none
}

// socket_read reads from socket into the provided byte pointer and length
fn (mut ws Client) socket_read_ptr(buf_ptr &u8, len int) ?int {
	lock  {
		if ws.state in [.closed, .closing] || ws.conn.sock.handle <= 1 {
			return error('socket_read_ptr: trying to read a closed socket')
		}
		if ws.is_ssl {
			r := ws.ssl_conn.socket_read_into_ptr(buf_ptr, len)?
			return r
		} else {
			r := ws.conn.read_ptr(buf_ptr, len)?
			return r
		}
	}
	return none
}

// socket_write writes the provided byte array to the socket
fn (mut ws Client) socket_write(bytes []u8) ?int {
	lock  {
		if ws.state == .closed || ws.conn.sock.handle <= 1 {
			ws.debug_log('socket_write: Socket allready closed')
			return error('socket_write: trying to write on a closed socket')
		}
		if ws.is_ssl {
			return ws.ssl_conn.write(bytes)
		} else {
			for {
				n := ws.conn.write(bytes) or {
					if err.code() == net.err_timed_out_code {
						continue
					}
					return err
				}
				return n
			}
			panic('reached unreachable code')
		}
	}
}

// shutdown_socket shuts down the socket properly when connection is closed
fn (mut ws Client) shutdown_socket() ? {
	ws.debug_log('shutting down socket')
	if ws.is_ssl {
		ws.ssl_conn.shutdown()?
	} else {
		ws.conn.close()?
	}
}

// dial_socket connects tcp socket and initializes default configurations
fn (mut ws Client) dial_socket() ?&net.TcpConn {
	tcp_address := '$ws.uri.hostname:$ws.uri.port'
	mut t := net.dial_tcp(tcp_address)?
	optval := int(1)
	t.sock.set_option_int(.keep_alive, optval)?
	t.set_read_timeout(ws.read_timeout)
	t.set_write_timeout(ws.write_timeout)
	if ws.is_ssl {
		ws.ssl_conn.connect(mut t, ws.uri.hostname)?
	}
	return t
}
