module smtp

import net
import time

fn smtp_timeout_test_server(mut listener net.TcpListener, done chan bool) {
	defer {
		listener.close() or {}
		done <- true
	}
	mut conn := listener.accept() or { panic(err) }
	defer {
		conn.close() or {}
	}
	conn.write_string('220 localhost ESMTP ready\r\n') or { panic(err) }
	ehlo := conn.read_line().trim_space()
	assert ehlo.starts_with('EHLO ')
	conn.write_string('250-localhost\r\n250 OK\r\n') or { panic(err) }
	quit := conn.read_line().trim_space()
	assert quit == 'QUIT'
	conn.write_string('221 Bye\r\n') or { panic(err) }
}

fn test_client_timeout_configures_tcp_conn() ! {
	timeout := 7 * time.second
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	done := chan bool{}
	spawn smtp_timeout_test_server(mut listener, done)
	mut client := new_client(Client{
		server:  '127.0.0.1'
		port:    int(port)
		timeout: timeout
	})!
	assert client.is_open
	assert client.conn.read_timeout() == timeout
	assert client.conn.write_timeout() == timeout
	client.quit()!
	assert !client.is_open
	_ := <-done
}
