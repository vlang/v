// vtest build: linux
// vtest vflags: -d fasthttp_test_delay_async_start
module fasthttp

import net
import time

fn lifecycle_test_handler(_ HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n'.bytes()
	}
}

fn new_lifecycle_test_server() !&Server {
	return new_server(ServerConfig{
		family:                  .ip
		port:                    0
		max_request_buffer_size: 8192
		handler:                 lifecycle_test_handler
	})
}

fn test_close_command_releases_unregistered_write_state() ! {
	server := new_lifecycle_test_server()!
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer {
		C.close(epoll_fd)
	}
	client_fd := int(C.socket(i32(net.AddrFamily.ip), i32(net.SocketType.tcp), 0))
	assert client_fd >= 0
	assert add_fd_to_epoll(epoll_fd, client_fd, u32(C.EPOLLIN)) == 0

	server.begin_request()
	state := &ClientWriteState{
		content:        'pending'.bytes()
		content_owned:  true
		request_active: true
	}
	mut client_fds := {
		client_fd: u64(1)
	}
	mut client_buffers := map[int][]u8{}
	mut client_read_starts := map[int]i64{}
	mut closing_client_fds := map[int]bool{}
	mut client_write_states := map[int]&ClientWriteState{}
	process_loop_command(server, epoll_fd, LoopCommand{
		kind:       .close_conn
		client_fd:  client_fd
		generation: 1
		state:      state
	}, mut client_fds, mut client_buffers, mut client_read_starts, mut closing_client_fds, mut
		client_write_states)

	assert server.active_request_count() == 0
	assert client_fd !in client_fds
	assert client_fd !in client_write_states
}

fn test_dispatch_marks_request_active_before_spawn() ! {
	server := new_lifecycle_test_server()!
	command_ch := chan LoopCommand{cap: 1}
	wakeup_fd := C.eventfd(0, C.EFD_NONBLOCK | C.EFD_CLOEXEC)
	assert wakeup_fd >= 0
	defer {
		C.close(wakeup_fd)
	}
	dispatch_request_async(server, -1, 1, 'invalid request'.bytes(), command_ch, wakeup_fd)
	assert server.active_request_count() == 1

	select {
		_ := <-command_ch {
			assert server.active_request_count() == 0
		}
		1 * time.second {
			assert false, 'timed out waiting for the async request to finish'
		}
	}
}

fn test_manual_takeover_keeps_reused_fd_state() ! {
	server := new_lifecycle_test_server()!
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer {
		C.close(epoll_fd)
	}
	client_fd := int(C.socket(i32(net.AddrFamily.ip), i32(net.SocketType.tcp), 0))
	assert client_fd >= 0
	defer {
		C.close(client_fd)
	}
	mut client_fds := {
		client_fd: u64(2)
	}
	mut client_buffers := {
		client_fd: 'GET /partial'.bytes()
	}
	mut client_read_starts := map[int]i64{}
	client_read_starts[client_fd] = time.sys_mono_now()
	mut closing_client_fds := {
		client_fd: true
	}
	mut client_write_states := map[int]&ClientWriteState{}
	server.begin_request()
	process_loop_command(server, epoll_fd, LoopCommand{
		kind:       .manual_takeover
		client_fd:  client_fd
		generation: 1
	}, mut client_fds, mut client_buffers, mut client_read_starts, mut closing_client_fds, mut
		client_write_states)

	assert server.active_request_count() == 0
	assert client_fds[client_fd] or { 0 } == 2
	assert client_buffers[client_fd] or { []u8{} } == 'GET /partial'.bytes()
	assert client_fd in client_read_starts
	assert closing_client_fds[client_fd] or { false }
}

fn test_loop_command_wakes_epoll() ! {
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer {
		C.close(epoll_fd)
	}
	wakeup_fd := C.eventfd(0, C.EFD_NONBLOCK | C.EFD_CLOEXEC)
	assert wakeup_fd >= 0
	defer {
		C.close(wakeup_fd)
	}
	assert add_fd_to_epoll(epoll_fd, wakeup_fd, u32(C.EPOLLIN)) == 0
	command_ch := chan LoopCommand{cap: 1}
	send_loop_command(command_ch, wakeup_fd, LoopCommand{
		kind:      .close_conn
		client_fd: -1
	})

	mut event := C.epoll_event{}
	watch := time.new_stopwatch()
	assert C.epoll_wait(epoll_fd, &event, 1, 1000) == 1
	assert watch.elapsed() < 250 * time.millisecond
	event_fd := unsafe { event.data.fd }
	assert event_fd == wakeup_fd
	drain_event_loop_wakeup(wakeup_fd)
	_ := <-command_ch
}
