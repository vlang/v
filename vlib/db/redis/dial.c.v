module redis

import net
import time

fn dial_connection(config Config) !&net.TcpConn {
	if config.connect_timeout <= 0 {
		mut conn := net.dial_tcp(endpoint(config)) or { return ConnectionError{ message: err.msg() } }
		net.set_blocking(conn.sock.handle, false) or {
			conn.close() or {}
			return ConnectionError{ message: err.msg() }
		}
		conn.is_blocking = false
		return conn
	}
	addresses := net.resolve_addrs_fuzzy(endpoint(config), .tcp) or {
		return ConnectionError{ message: err.msg() }
	}
	deadline := time.now().add(config.connect_timeout)
	mut last_error := 'connection failed'
	for address in addresses {
		mut socket := net.new_tcp_socket(address.family()) or {
			last_error = err.msg()
			continue
		}
		mut conn := &net.TcpConn{ sock: socket, is_blocking: false }
		net.set_blocking(socket.handle, false) or {
			conn.close() or {}
			return ConnectionError{ message: err.msg() }
		}
		// A nonblocking connect and select bound the TCP handshake without a background dial leak.
		status := C.connect(socket.handle, voidptr(&address), address.len())
		if status != 0 {
			connect_code := net.error_code()
			if connect_code !in [net.error_einprogress, net.error_ewouldblock, net.error_eagain,
				net.error_eintr] {
				conn.close() or {}
				last_error = 'TCP connect failed (${connect_code})'
				continue
			}
			remaining := deadline - time.now()
			if remaining <= 0 {
				conn.close() or {}
				return ConnectionError{ message: 'connect timeout' }
			}
			mut set := C.fd_set{}
			C.FD_ZERO(&set)
			C.FD_SET(socket.handle, &set)
			timeout := C.timeval{
				tv_sec:  u64(remaining / time.second)
				tv_usec: u64((remaining % time.second).microseconds())
			}
			ready := C.select(socket.handle + 1, unsafe { nil }, &set, unsafe { nil }, &timeout)
			mut code := i32(0)
			mut length := u32(sizeof(code))
			if ready <= 0 || C.getsockopt(socket.handle, C.SOL_SOCKET, C.SO_ERROR, &code, &length) != 0 || code != 0 {
				conn.close() or {}
				last_error = if ready == 0 {
					'connect timeout'
				} else {
					'TCP connect failed (${code})'
				}
				continue
			}
		}
		return conn
	}
	return ConnectionError{ message: last_error }
}
