// vtest build: linux
module fasthttp

fn C.socketpair(domain i32, typ i32, protocol i32, sockets &i32) i32

fn linux_reregistration_test_handler(_ HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n'.bytes()
	}
}

fn test_keep_alive_completion_rearms_epollin_after_consumed_edge() ! {
	server := new_server(ServerConfig{
		family:                  .ip
		port:                    0
		max_request_buffer_size: 8192
		handler:                 linux_reregistration_test_handler
	})!
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer {
		C.close(epoll_fd)
	}
	mut sockets := [2]int{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	defer {
		C.close(sockets[0])
		C.close(sockets[1])
	}
	set_blocking(sockets[0], false)
	assert add_fd_to_epoll(epoll_fd, sockets[0], u32(C.EPOLLIN | C.EPOLLET)) == 0
	assert C.write(sockets[1], c'GET ', 4) == 4

	mut event := C.epoll_event{}
	assert C.epoll_wait(epoll_fd, &event, 1, 1000) == 1
	assert unsafe { event.data.fd } == sockets[0]

	server.begin_request()
	mut state := &ClientWriteState{
		request_active: true
	}
	mut client_fds := {
		sockets[0]: true
	}
	mut client_buffers := map[int][]u8{}
	mut client_read_starts := map[int]i64{}
	mut closing_client_fds := map[int]bool{}
	mut client_write_states := {
		sockets[0]: state
	}
	complete_write(server, epoll_fd, sockets[0], mut client_fds, mut client_buffers, mut
		client_read_starts, mut closing_client_fds, mut client_write_states)

	assert server.active_request_count() == 0
	assert sockets[0] in client_fds
	assert C.epoll_wait(epoll_fd, &event, 1, 1000) == 1
	assert unsafe { event.data.fd } == sockets[0]
}
