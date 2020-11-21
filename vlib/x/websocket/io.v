module websocket

import net
import time

interface WebsocketIO {
	socket_read(mut buffer []byte) ?int
	socket_write(bytes []byte) ?
}

// socket_read reads into the provided buffer with its length
fn (mut ws Client) socket_read(mut buffer []byte) ?int {
	lock  {
		if ws.state in [.closed, .closing] || ws.conn.sock.handle <= 1 {
			return error('socket_read: trying to read a closed socket')
		}
		if ws.is_ssl {
			r := ws.ssl_conn.read_into(mut buffer)?
			return r
		} else {
			for {
				r := ws.conn.read(mut buffer) or {
					if errcode == net.err_timed_out_code {
						continue
					}
					return error(err)
				}
				return r
			}
		}
	}
}

fn (mut ws Client) socket_read_ptr(buf_ptr byteptr, len int) ?int {
	lock  {
		if ws.state in [.closed, .closing] || ws.conn.sock.handle <= 1 {
			return error('socket_read_ptr: trying to read a closed socket')
		}	
		
		if ws.is_ssl {
			r := ws.ssl_conn.socket_read_into_ptr(buf_ptr, len)?
			return r
		} else {
			for {
				r := ws.conn.read_ptr(buf_ptr, len) or {
					if errcode == net.err_timed_out_code {
						continue
					}
					return error(err)
				}
				return r
			}
		}
	}
}

// socket_write, writes the whole byte array provided to the socket
fn (mut ws Client) socket_write(bytes []byte) ? {
	lock  {
		if ws.state == .closed || ws.conn.sock.handle <= 1 {
			ws.debug_log('socket_write: Socket allready closed')
			return error('socket_write: trying to write on a closed socket')
		}
		if ws.is_ssl {
			ws.ssl_conn.write(bytes)?
		} else {
			for {
				ws.conn.write(bytes) or {
					if errcode == net.err_timed_out_code {
						continue
					}
					return error(err)
				}
				return
			}
		}
	}
}

// shutdown_socket, proper shutdown make PR in Emeliy repo
fn (mut ws Client) shutdown_socket() ? {
	ws.debug_log('shutting down socket')
	if ws.is_ssl {
		ws.ssl_conn.shutdown()?
	} else {
		ws.conn.close()?
	}
	return none
}

// dial_socket, setup socket communication, options and timeouts
fn (mut ws Client) dial_socket() ?net.TcpConn {
	mut t := net.dial_tcp('$ws.uri.hostname:$ws.uri.port')?
	optval := int(1)
	t.sock.set_option_int(.keep_alive, optval)?
	t.set_read_timeout(10 * time.millisecond)
	t.set_write_timeout(10 * time.millisecond)
	if ws.is_ssl {
		ws.ssl_conn.connect(mut t)?
	}
	return t
}
