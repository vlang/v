// vtest build: linux && !tinyc
// vtest vflags: -prealloc
module fasthttp

import time

fn C.socketpair(domain i32, typ i32, protocol i32, sockets &i32) i32

fn prealloc_async_state_test_handler(_ HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nOK'.bytes()
	}
}

fn test_prealloc_async_write_state_outlives_worker() ! {
	server := new_server(ServerConfig{
		family:                  .ip
		port:                    0
		max_request_buffer_size: 8192
		handler:                 prealloc_async_state_test_handler
	})!
	mut sockets := [2]int{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	defer {
		C.close(sockets[0])
		C.close(sockets[1])
	}
	wakeup_fd := C.eventfd(0, C.EFD_NONBLOCK | C.EFD_CLOEXEC)
	assert wakeup_fd >= 0
	defer {
		C.close(wakeup_fd)
	}
	command_ch := chan LoopCommand{cap: 1}
	server.begin_request()
	worker := spawn process_request_async(server, sockets[0], 1,
		'GET / HTTP/1.1\r\nHost: localhost\r\n\r\n'.bytes(), command_ch, wakeup_fd)

	mut cmd := LoopCommand{}
	select {
		command := <-command_ch {
			cmd = command
		}
		1 * time.second {
			assert false, 'timed out waiting for the async response'
			return
		}
	}
	worker.wait()
	assert cmd.kind == .complete_response
	assert cmd.state != unsafe { nil }
	assert cmd.state.content.len > 0

	mut states := map[int]&ClientWriteState{}
	states[sockets[0]] = cmd.state
	free_write_state(server, sockets[0], mut states)
	assert server.active_request_count() == 0
}
