module net

import os
import time

fn C.pthread_kill(thread uint, sig i32) i32

fn read_line_interrupt_handler(_ os.Signal) {}

fn read_line_interrupted_server(mut listener TcpListener, ready chan uint, result chan string, done chan bool) {
	mut conn := listener.accept() or { panic(err) }
	defer { conn.close() or {} }
	conn.set_read_timeout(2 * time.second)
	ready <- C.pthread_self()
	result <- conn.read_line()
	// Keep the thread alive until the sender has finished delivering signals.
	_ := <-done
}

fn test_tcp_read_line_retries_interrupted_receive() {
	previous := os.signal_opt(.usr1, read_line_interrupt_handler)!
	defer { os.signal_opt(.usr1, previous) or {} }
	mut listener := listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	address := listener.addr()!
	ready := chan uint{cap: 1}
	result := chan string{cap: 1}
	done := chan bool{cap: 1}
	server := spawn read_line_interrupted_server(mut listener, ready, result, done)
	mut client := dial_tcp(address.str())!
	defer { client.close() or {} }
	tid := <-ready
	for _ in 0 .. 3 {
		time.sleep(10 * time.millisecond)
		assert C.pthread_kill(tid, C.SIGUSR1) == 0
	}
	client.write_string('still connected\r\n')!
	line := <-result
	done <- true
	server.wait()
	assert line == 'still connected\r\n'
}
