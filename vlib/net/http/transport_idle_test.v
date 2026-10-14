module http

import io
import net
import time

fn test_idle_probe_does_not_consume_pending_bytes() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	mut client := net.dial_tcp(listener.addr()!.str())!
	defer {
		client.close() or {}
	}
	mut server := listener.accept()!
	mut server_closed := false
	defer {
		if !server_closed {
			server.close() or {}
		}
	}
	pooled := &H1PooledConn{
		tcp: client
	}
	assert !pooled.idle_tcp_closed()
	server.write_string('pending')!
	client.set_read_timeout(time.second)
	client.wait_for_read()!
	assert !pooled.idle_tcp_closed()
	mut buf := []u8{len: 1}
	mut received := []u8{}
	for _ in 0 .. 7 {
		assert client.read(mut buf)! == 1
		received << buf[0]
	}
	assert received.bytestr() == 'pending'
	server.close()!
	server_closed = true
	if _ := client.read(mut buf) {
		assert false, 'expected EOF after the peer closed'
	} else {
		assert err is io.Eof
	}
	assert pooled.idle_tcp_closed()
}
