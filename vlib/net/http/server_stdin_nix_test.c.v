module http

import net
import time

#include <unistd.h>

fn C.dup(int) int
fn C.dup2(int, int) int
fn C.close(int) int

fn stop_stdin_test_server(mut server Server) {
	assert server.listener.sock.handle != 0
	server.stop()
}

fn stop_explicit_stdin_test_server(mut server Server) {
	assert server.listener.sock.handle == 0
	server.stop()
}

fn test_server_does_not_use_a_socket_on_stdin() {
	saved_stdin := C.dup(0)
	assert saved_stdin >= 0
	mut other := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer {
		assert C.dup2(saved_stdin, 0) == 0
		C.close(saved_stdin)
		other.close() or {}
	}
	other_addr := other.addr()!.str()
	assert C.dup2(other.sock.handle, 0) == 0
	mut server := &Server{
		addr:                 '127.0.0.1:0'
		worker_num:           1
		accept_timeout:       10 * time.millisecond
		show_startup_message: false
		on_running:           stop_stdin_test_server
	}
	server.listen_and_serve()
	assert server.addr != other_addr
	assert server.listener.sock.handle == -1
	assert server.status() == .closed
}

fn test_server_accepts_an_explicit_listener_on_stdin() {
	saved_stdin := C.dup(0)
	assert saved_stdin >= 0
	mut other := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer {
		assert C.dup2(saved_stdin, 0) == 0
		C.close(saved_stdin)
		other.close() or {}
	}
	other_addr := other.addr()!.str()
	assert C.dup2(other.sock.handle, 0) == 0
	mut server := &Server{
		listener:             net.TcpListener{
			sock: net.TcpSocket{
				Socket: net.Socket{
					handle: 0
				}
			}
		}
		addr:                 '127.0.0.1:0'
		worker_num:           1
		accept_timeout:       10 * time.millisecond
		show_startup_message: false
		on_running:           stop_explicit_stdin_test_server
	}
	server.listen_and_serve()
	assert server.addr == other_addr
	assert server.listener.sock.handle == -1
	assert server.status() == .closed
}
