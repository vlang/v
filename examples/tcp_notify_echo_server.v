import os
import os.notify
import net
import time

// This example demonstrates a single threaded TCP server using os.notify to be
// notified of events on file descriptors. You can connect to the server using
// netcat in separate shells, for example: `nc localhost 9001`

fn main() {
	$if !linux {
		eprintln('This example only works on Linux')
		exit(1)
	}

	// create TCP listener
	mut listener := net.listen_tcp(.ip, 'localhost:9001')!
	defer {
		listener.close() or {}
	}
	addr := listener.addr()!
	eprintln('Listening on ${addr}')
	eprintln('Type `stop` to stop the server')

	// create file descriptor notifier
	mut notifier := notify.new()!
	defer {
		notifier.close() or {}
	}
	notifier.add(os.stdin().fd, .read)!
	notifier.add(listener.sock.handle, .read)!

	for {
		for event in notifier.wait(time.infinite) {
			match event.fd {
				listener.sock.handle {
					// someone is trying to connect
					eprint('trying to connect.. ')
					if conn := listener.accept() {
						if _ := notifier.add(conn.sock.handle, .read | .peer_hangup) {
							eprintln('connected')
						} else {
							eprintln('error adding to notifier: ${err}')
						}
					} else {
						eprintln('unable to accept: ${err}')
					}
				}
				0 {
					// stdin
					s, _ := os.fd_read(event.fd, 10)
					if s == 'stop\n' {
						eprintln('stopping')
						return
					}
				}
				else {
					// remote connection
					if event.kind.has(.peer_hangup) {
						if _ := notifier.remove(event.fd) {
							eprintln('remote disconnected')
						} else {
							eprintln('error removing from notifier: ${err}')
						}
					} else {
						s, _ := os.fd_read(event.fd, 10)
						os.fd_write(event.fd, s)
					}
				}
			}
		}
	}
}
