module pop

import net

// run_fake_server serves one scripted POP3 session: greeting, USER/PASS,
// STAT, LIST, RETR, DELE, QUIT. Each test binds its own ephemeral port.
fn run_fake_server(ready chan int) {
	mut server := net.listen_tcp(.ip, '127.0.0.1:0') or { panic('listen: ${err}') }
	defer {
		server.close() or {}
	}
	address := server.addr() or { panic(err) }
	port := address.port() or { panic(err) }
	ready <- int(port)
	mut conn := server.accept() or { panic('accept: ${err}') }
	defer {
		conn.close() or {}
	}
	conn.write_string('+OK fake-pop3 ready\r\n') or { panic('greet: ${err}') }
	for {
		// TcpConn.read_line keeps the line ending; strip it for matching.
		line := conn.read_line().trim_right('\r\n')
		if line == '' {
			break
		}
		cmd := if line.contains(' ') { line.all_before(' ') } else { line }
		match cmd {
			'USER', 'PASS', 'DELE' {
				conn.write_string('+OK done\r\n') or { break }
			}
			'STAT' {
				conn.write_string('+OK 2 320\r\n') or { break }
			}
			'LIST' {
				conn.write_string('+OK scan follows\r\n1 120\r\n2 200\r\n.\r\n') or {
					break
				}
			}
			'RETR' {
				conn.write_string('+OK 120 octets\r\nSubject: hi\r\n..dot-stuffed\r\nplain\r\n.\r\n') or {
					break
				}
			}
			'QUIT' {
				conn.write_string('+OK bye\r\n') or {}
				break
			}
			else {
				conn.write_string('-ERR unknown command\r\n') or { break }
			}
		}
	}
}

fn start_session() &Client {
	ready := chan int{cap: 1}
	defer {
		ready.close()
	}
	spawn run_fake_server(ready)
	port := <-ready
	mut c := dial(Config{
		server:   '127.0.0.1'
		port:     port
		username: 'user'
		password: 'pass'
	}) or { panic('dial: ${err}') }
	c.login() or { panic('login: ${err}') }
	return c
}

fn test_pop_login_and_stat() {
	mut c := start_session()
	defer {
		c.quit() or { panic('quit: ${err}') }
	}
	n, octets := c.stat()!
	assert n == 2
	assert octets == 320
}

fn test_pop_list_retr_dele() {
	mut c := start_session()
	defer {
		c.quit() or { panic('quit: ${err}') }
	}
	listing := c.list_all()!
	assert listing.contains('1 120')
	assert listing.contains('2 200')
	msg := c.retr(1)!
	assert msg.contains('Subject: hi')
	assert msg.contains('.dot-stuffed'), 'dot-stuffing must be undone'
	assert !msg.contains('..dot-stuffed')
	assert msg.contains('plain')
	c.dele(1)!
}

fn test_pop_dial_refused_errors() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	address := listener.addr()!
	port := int(address.port()!)
	listener.close()!
	dial(Config{
		server: '127.0.0.1'
		port:   port
	}) or {
		return
	}
	assert false, 'dial to a closed port must error'
}

fn test_pop_rejects_injected_credentials_before_connecting() {
	for value in ['user\r\nQUIT', 'user\nPASS x', 'user\x00suffix'] {
		if _ := dial(Config{ server: '127.0.0.1', port: 1, username: value }) {
			assert false
		} else {
			assert err.msg().contains('credentials')
		}
		if _ := dial(Config{ server: '127.0.0.1', port: 1, password: value }) {
			assert false
		} else {
			assert err.msg().contains('credentials')
		}
	}
}

fn test_pop_closed_client_and_invalid_message_numbers_return_errors() {
	mut c := Client{}
	if _, _ := c.stat() {
		assert false
	} else {
		assert err.msg().contains('closed')
	}
	if _ := c.retr(0) {
		assert false
	} else {
		assert err.msg().contains('positive')
	}
	if _ := c.dele(-1) {
		assert false
	} else {
		assert err.msg().contains('positive')
	}
	c.close()!
}

fn test_pop_status_tokens_are_delimited() {
	c := Client{}
	assert c.expect_ok('+OK')! == '+OK'
	if _ := c.expect_ok('+OKAY bad') {
		assert false
	}
	if _ := c.expect_ok('unexpected') {
		assert false
	}
}

fn run_failure_server(ready chan int, greeting string, response string) {
	mut server := net.listen_tcp(.ip, '127.0.0.1:0') or { panic(err) }
	defer { server.close() or {} }
	address := server.addr() or { panic(err) }
	ready <- int(address.port() or { panic(err) })
	mut conn := server.accept() or { panic(err) }
	defer { conn.close() or {} }
	conn.write_string(greeting + '\r\n') or { return }
	if greeting.starts_with('+OK') {
		_ := conn.read_line()
		conn.write_string(response) or {}
	}
}

fn failure_session(response string) &Client {
	ready := chan int{cap: 1}
	spawn run_failure_server(ready, '+OK ready', response)
	port := <-ready
	return dial(Config{ server: '127.0.0.1', port: port }) or { panic(err) }
}

fn test_pop_malformed_stat_numbers_are_errors() {
	for response in ['+OK x 32', '+OK 2 -1', '+OK 1 2147483648', '+OK 1 99999999999999999999999999999'] {
		mut c := failure_session(response + '\r\n')
		defer { c.close() or {} }
		if _, _ := c.stat() {
			assert false, response
		}
	}
}

fn test_pop_rejects_invalid_connection_settings() {
	for server in ['', '127.0.0.1\x00.invalid', '[::1]:110', '[::1', '::1]', '[localhost]'] {
		if _ := dial(Config{ server: server }) {
			assert false
		} else {
			assert err.msg().contains('host name')
		}
	}
	for port in [-1, 0, 65536] {
		if _ := dial(Config{ server: '127.0.0.1', port: port }) {
			assert false
		} else {
			assert err.msg().contains('port must be')
		}
	}
}

fn test_pop_quit_failure_still_closes_connection() {
	mut c := failure_session('-ERR cannot quit\r\n')
	if _ := c.quit() {
		assert false
	}
	assert !c.opened
	c.close()!
}

fn test_pop_truncated_multiline_closes_connection() {
	mut c := failure_session('+OK message follows\r\nno terminator\r\n')
	if _ := c.retr(1) {
		assert false
	}
	assert !c.opened
}

fn test_pop_invalid_greeting_returns_error() {
	ready := chan int{cap: 1}
	spawn run_failure_server(ready, '-ERR unavailable', '')
	port := <-ready
	if _ := dial(Config{ server: '127.0.0.1', port: port }) {
		assert false
	}
}
