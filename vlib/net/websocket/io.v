module websocket

fn (mut ws Client) write_to_server(buf voidptr, len int) int {
	mut bytes_written := 0
	ws.write_lock.m_lock()
	if ws.is_ssl { 
		bytes_written = C.SSL_write(ws.ssl, buf, len)
	} else {
		bytes_written = C.write(ws.socket.sockfd, buf, len)
	}        
	ws.write_lock.unlock()
	return bytes_written
}

fn (ws &Client) read_from_server(buffer byteptr, buffer_size int) int {
	mut res := 0
	if ws.is_ssl {
		res = C.SSL_read(ws.ssl, buffer, buffer_size)
	} else {
		res = C.read(ws.socket.sockfd, buffer, buffer_size)
	}
	return res
}
