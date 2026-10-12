module http

import net
import time

fn assert_restart_test_server_bound(server &Server) {
	assert server.status() == .running
	assert server.addr.starts_with('127.0.0.1:')
	assert !server.addr.ends_with(':0')
	assert server.listener.addr() or { panic(err) }.str() == server.addr
}

fn stop_restart_test_server(mut server Server) {
	assert_restart_test_server_bound(server)
	server.stop()
}

fn close_restart_test_server(mut server Server) {
	assert_restart_test_server_bound(server)
	server.close()
}

fn test_server_can_restart_after_stop() {
	mut server := &Server{
		addr:                 '127.0.0.1:0'
		worker_num:           1
		accept_timeout:       10 * time.millisecond
		show_startup_message: false
		on_running:           stop_restart_test_server
	}
	for _ in 0 .. 3 {
		server.listen_and_serve()
		assert server.status() == .closed
	}
}

fn test_server_can_restart_after_close() {
	mut server := &Server{
		addr:                 '127.0.0.1:0'
		worker_num:           1
		accept_timeout:       10 * time.millisecond
		show_startup_message: false
		on_running:           close_restart_test_server
	}
	for _ in 0 .. 3 {
		server.listen_and_serve()
		assert server.status() == .closed
	}
}

fn test_server_can_restart_after_closing_an_explicit_listener() {
	listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	mut server := &Server{
		listener:             listener
		worker_num:           1
		accept_timeout:       10 * time.millisecond
		show_startup_message: false
		on_running:           close_restart_test_server
	}
	for _ in 0 .. 3 {
		server.listen_and_serve()
		assert server.status() == .closed
	}
}
