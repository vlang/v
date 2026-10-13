module pop

import net

const test_port = 22591

// run_fake_server serves one scripted POP3 session: greeting, USER/PASS,
// STAT, LIST, RETR, DELE, QUIT. A second connection attempt would block, so
// each test using it gets its own port offset.
fn run_fake_server(ready chan bool, port int) {
	mut server := net.listen_tcp(.ip, ':${port}') or { panic('listen: ${err}') }
	defer {
		server.close() or {}
	}
	ready <- true
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
	ready := chan bool{cap: 1}
	defer {
		ready.close()
	}
	spawn run_fake_server(ready, test_port)
	_ := <-ready
	mut c := dial(Config{
		server:   '127.0.0.1'
		port:     test_port
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
	dial(Config{
		server: '127.0.0.1'
		port:   1
	}) or {
		return
	}
	assert false, 'dial to a closed port must error'
}
