module websocket

fn C.write() int

fn (mut ws Client) write_to_server(buf voidptr, len int) int {
	mut bytes_written := 0
	ws.write_lock.lock()
	bytes_written = if ws.is_ssl {
		C.SSL_write(ws.ssl, buf, len)
	} else {
		C.write(ws.socket.sockfd, buf, len)
	}
	ws.write_lock.unlock()
	return bytes_written
}

fn (ws &Client) read_from_server(buffer byteptr, buffer_size int) int {
	return if ws.is_ssl {
		C.SSL_read(ws.ssl, buffer, buffer_size)
	} else {
		C.read(ws.socket.sockfd, buffer, buffer_size)
	}
}
