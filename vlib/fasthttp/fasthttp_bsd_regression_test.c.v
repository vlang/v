// vtest build: macos || freebsd || openbsd || netbsd || dragonfly
module fasthttp

fn C.socketpair(domain i32, typ i32, protocol i32, sockets &i32) i32

fn bsd_reregistration_test_handler(_ HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n'.bytes()
	}
}

fn test_keep_alive_completion_rearms_kqueue_read_after_consumed_edge() ! {
	server := new_server(ServerConfig{
		family:                  .ip
		port:                    0
		max_request_buffer_size: 8192
		handler:                 bsd_reregistration_test_handler
	})!
	kq := C.kqueue()
	assert kq >= 0
	defer {
		C.close(kq)
	}
	mut sockets := [2]int{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	defer {
		C.close(sockets[0])
		C.close(sockets[1])
	}
	set_nonblocking(sockets[0])
	mut conn := &Conn{
		fd:             sockets[0]
		request_active: true
		file_fd:        -1
	}
	assert add_event(kq, u64(sockets[0]), i16(C.EVFILT_READ),
		u16(C.EV_ADD | C.EV_ENABLE | C.EV_CLEAR), conn) == 0
	assert C.write(sockets[1], c'GET ', 4) == 4

	mut event := C.kevent{}
	mut timeout := C.timespec{
		tv_sec: 1
	}
	assert C.kevent(kq, unsafe { nil }, 0, &event, 1, &timeout) == 1
	assert event.ident == u64(sockets[0])

	server.begin_request()
	mut clients := {
		sockets[0]: voidptr(conn)
	}
	complete_response(server, kq, conn, mut clients, false)

	assert server.active_request_count() == 0
	assert clients[sockets[0]] or { unsafe { nil } } == voidptr(conn)
	timeout = C.timespec{
		tv_sec: 1
	}
	assert C.kevent(kq, unsafe { nil }, 0, &event, 1, &timeout) == 1
	assert event.ident == u64(sockets[0])
	close_conn(server, kq, conn, mut clients)
}
