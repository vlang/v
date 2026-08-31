module smtp

import io
import net

// smtp_recv_rcpts serves a minimal SMTP handshake and forwards all received
// `RCPT TO:` lines back via the result channel, joined by '\n'.
fn smtp_recv_rcpts(mut listener net.TcpListener, result chan string) {
	defer {
		listener.close() or {}
	}
	mut conn := listener.accept() or { return }
	defer {
		conn.close() or {}
	}
	conn.write_string('220 localhost ESMTP ready\r\n') or { return }
	_ := conn.read_line() // drain EHLO
	conn.write_string('250 OK\r\n') or { return }

	mut got := []string{}
	mut reader := io.new_buffered_reader(reader: conn)
	for {
		line := reader.read_line() or { break }
		got << line
		conn.write_string('250 OK\r\n') or { break }
	}
	result <- got.join('\n')
}

// send_mailto must skip empty/whitespace-only ';'-separated recipients (no
// `RCPT TO:<>`) and strip display names down to the bare mailbox.
fn test_send_mailto_skips_empty_recipients() ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	result := chan string{}

	spawn smtp_recv_rcpts(mut listener, result)
	mut client := new_client(Config{ server: '127.0.0.1', port: int(port) })!
	client.send_mailto(';;User <a@ex.com> ;   ; b@ex.com;')!
	client.conn.close() or {}

	mut expected := ['RCPT TO:<a@ex.com>', 'RCPT TO:<b@ex.com>']
	received := <-result
	assert received == expected.join('\n')
}
