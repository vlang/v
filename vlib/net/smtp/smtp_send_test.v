module smtp

import io
import net

// smtp_recv_send serves a full send() round: EHLO, MAIL FROM, one or more
// RCPT TO, DATA + message body, then the final 250. All received `RCPT TO:`
// lines are forwarded back via the result channel, joined by '\n'.
fn smtp_recv_send(mut listener net.TcpListener, result chan string) {
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
	mut in_data := false
	mut reader := io.new_buffered_reader(reader: conn)
	for {
		line := reader.read_line() or { break }
		if line == 'DATA' {
			conn.write_string('354 End data with <CR><LF>.<CR><LF>\r\n') or { break }
			in_data = true
			continue
		}
		if in_data {
			if line == '.' {
				conn.write_string('250 OK\r\n') or { break }
				break
			}
			// message body
			continue
		}
		if line.starts_with('RCPT TO:') {
			got << line
		}
		conn.write_string('250 OK\r\n') or { break }
	}
	result <- got.join('\n')
}

// send() must turn every recipient class (To, Cc, Bcc) into an envelope RCPT
// TO, skipping empty ';'-separated entries (no `RCPT TO:<>`) and stripping
// display names down to the bare mailbox.
fn test_send_includes_to_cc_and_bcc_in_envelope() ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	result := chan string{}

	spawn smtp_recv_send(mut listener, result)
	mut client := new_client(Config{ server: '127.0.0.1', port: int(port) })!
	// To: with empty entries and a display name; Cc and Bcc as extra classes.
	client.send(Mail{
		from: 'sender@example.com'
		to: ' ;;User <a@ex.com> ;   ; b@ex.com; '
		cc: ' ;; cc@ex.com ; ; '
		bcc: ' ;; Bcc Two <bcc@ex.com> ; ; '
		subject: 'test'
	})!
	client.conn.close() or {}

	expected := [
		'RCPT TO:<a@ex.com>',
		'RCPT TO:<b@ex.com>',
		'RCPT TO:<cc@ex.com>',
		'RCPT TO:<bcc@ex.com>',
	]
	received := <-result
	assert received == expected.join('\n')
}
