// vtest build: macos || freebsd || openbsd || netbsd || dragonfly
module fasthttp

import net
import time

fn bsd_lifecycle_test_handler(_ HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n'.bytes()
	}
}

fn new_bsd_lifecycle_test_server() !&Server {
	return new_server(ServerConfig{
		family:                  .ip
		port:                    0
		max_request_buffer_size: 8192
		handler:                 bsd_lifecycle_test_handler
	})
}

fn test_reusable_completion_ends_request_in_event_loop() ! {
	server := new_bsd_lifecycle_test_server()!
	kq := C.kqueue()
	assert kq >= 0
	defer {
		C.close(kq)
	}
	client_fd := int(C.socket(i32(net.AddrFamily.ip), i32(net.SocketType.tcp), 0))
	assert client_fd >= 0
	mut conn := &Conn{
		fd:             client_fd
		request_active: true
		processing:     true
		file_fd:        -1
	}
	mut clients := {
		client_fd: voidptr(conn)
	}
	server.begin_request()
	process_loop_command(server, kq, LoopCommand{
		kind:  .enable_read
		c_ptr: conn
	}, mut clients)

	assert server.active_request_count() == 0
	assert !conn.request_active
	assert !conn.processing
	close_conn(server, kq, conn, mut clients)
}

fn test_manual_takeover_keeps_reused_fd_client_tracked() ! {
	server := new_bsd_lifecycle_test_server()!
	kq := C.kqueue()
	assert kq >= 0
	defer {
		C.close(kq)
	}
	client_fd := int(C.socket(i32(net.AddrFamily.ip), i32(net.SocketType.tcp), 0))
	assert client_fd >= 0
	mut old_conn := &Conn{
		fd:             client_fd
		request_active: true
		processing:     true
		file_fd:        -1
	}
	mut new_conn := &Conn{
		fd:      client_fd
		file_fd: -1
	}
	mut clients := {
		client_fd: voidptr(new_conn)
	}
	server.begin_request()
	process_loop_command(server, kq, LoopCommand{
		kind:  .manual_takeover
		c_ptr: old_conn
	}, mut clients)

	assert server.active_request_count() == 0
	assert clients[client_fd] or { unsafe { nil } } == voidptr(new_conn)
	clients.delete(client_fd)
	C.close(client_fd)
	unsafe { free(new_conn) }
}

fn test_loop_command_wakes_kqueue() ! {
	kq := C.kqueue()
	assert kq >= 0
	defer {
		C.close(kq)
	}
	assert add_event(kq, kqueue_command_ident, i16(C.EVFILT_USER), u16(C.EV_ADD | C.EV_CLEAR),
		unsafe { nil }) == 0
	command_ch := chan LoopCommand{cap: 1}
	send_loop_command(command_ch, kq, LoopCommand{
		kind: .close_conn
	})

	mut event := C.kevent{}
	timeout := C.timespec{
		tv_sec: 1
	}
	watch := time.new_stopwatch()
	assert C.kevent(kq, unsafe { nil }, 0, &event, 1, &timeout) == 1
	assert watch.elapsed() < 250 * time.millisecond
	assert event.filter == i16(C.EVFILT_USER)
	assert event.ident == kqueue_command_ident
	_ := <-command_ch
}
