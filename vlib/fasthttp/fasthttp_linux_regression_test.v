// vtest build: linux
module fasthttp

// White-box tests for the epoll reactor's connection layer: pooled ConnState
// with reused buffers, HTTP/1.1 pipelining (many requests answered in one send),
// TCP-fragmented request reassembly, and keep-alive re-arm after a consumed edge.
// They drive the internal serve_conn / flush / pooling paths directly over a real
// socketpair, so no listener or port is needed.

fn C.socketpair(domain i32, typ i32, protocol i32, sockets &i32) i32

fn regression_handler(_ HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: keep-alive\r\n\r\nok'.bytes()
	}
}

fn new_regression_worker(server &Server, epoll_fd int) Worker {
	mut w := Worker{
		epoll_fd:  epoll_fd
		listen_fd: -1
		conns:     []&ConnState{len: conn_table_min, init: unsafe { nil }}
	}
	unsafe {
		w.server = server
	}
	return w
}

// recv_available drains whatever is currently readable on a non-blocking fd.
fn recv_available(fd int) string {
	mut out := []u8{}
	mut buf := [4096]u8{}
	for _ in 0 .. 64 {
		n := C.recv(fd, &buf[0], 4096, 0)
		if n <= 0 {
			break
		}
		unsafe { out.push_many(&buf[0], n) }
		if n < 4096 {
			break
		}
	}
	return out.bytestr()
}

fn test_pipelined_requests_answered_in_one_batch() ! {
	server := new_server(ServerConfig{
		family:  .ip
		port:    0
		handler: regression_handler
	})!
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer { C.close(epoll_fd) }
	mut sockets := [2]int{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	server_fd := sockets[0]
	client_fd := sockets[1]
	set_blocking(server_fd, false)
	set_blocking(client_fd, false)

	mut w := new_regression_worker(server, epoll_fd)
	assert add_fd_to_epoll(epoll_fd, server_fd, u32(C.EPOLLIN | C.EPOLLET)) == 0
	mut cs := state_for(mut w, server_fd)

	// Three pipelined requests in one write.
	req := 'GET /a HTTP/1.1\r\nHost: x\r\n\r\nGET /b HTTP/1.1\r\nHost: x\r\n\r\nGET /c HTTP/1.1\r\nHost: x\r\n\r\n'
	assert C.send(client_fd, req.str, req.len, C.MSG_NOSIGNAL) == req.len

	serve_conn(mut w, server_fd, mut cs)

	// All three responses come back, and the connection stays alive (keep-alive).
	resp := recv_available(client_fd)
	assert resp.count('HTTP/1.1 200 OK') == 3, resp
	assert unsafe { w.conns[server_fd] != nil }
	assert server.active_request_count() == 0

	C.close(server_fd)
	C.close(client_fd)
}

fn test_fragmented_request_is_reassembled() ! {
	server := new_server(ServerConfig{
		family:  .ip
		port:    0
		handler: regression_handler
	})!
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer { C.close(epoll_fd) }
	mut sockets := [2]int{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	server_fd := sockets[0]
	client_fd := sockets[1]
	set_blocking(server_fd, false)
	set_blocking(client_fd, false)

	mut w := new_regression_worker(server, epoll_fd)
	assert add_fd_to_epoll(epoll_fd, server_fd, u32(C.EPOLLIN | C.EPOLLET)) == 0
	mut cs := state_for(mut w, server_fd)

	// First fragment: an incomplete request head.
	part1 := 'GET /split HTTP/1.1\r\n'
	assert C.send(client_fd, part1.str, part1.len, C.MSG_NOSIGNAL) == part1.len
	serve_conn(mut w, server_fd, mut cs)
	// Nothing answered yet; the partial request is buffered and armed for timeout.
	assert cs.read_buf.len == part1.len
	assert recv_available(client_fd) == ''

	// Second fragment completes the request.
	part2 := 'Host: x\r\n\r\n'
	assert C.send(client_fd, part2.str, part2.len, C.MSG_NOSIGNAL) == part2.len
	serve_conn(mut w, server_fd, mut cs)
	resp := recv_available(client_fd)
	assert resp.contains('HTTP/1.1 200 OK'), resp
	assert cs.read_buf.len == 0 // fully consumed

	C.close(server_fd)
	C.close(client_fd)
}

fn test_keep_alive_rearms_after_consumed_edge() ! {
	server := new_server(ServerConfig{
		family:  .ip
		port:    0
		handler: regression_handler
	})!
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer { C.close(epoll_fd) }
	mut sockets := [2]int{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	server_fd := sockets[0]
	client_fd := sockets[1]
	set_blocking(server_fd, false)
	set_blocking(client_fd, false)

	mut w := new_regression_worker(server, epoll_fd)
	assert add_fd_to_epoll(epoll_fd, server_fd, u32(C.EPOLLIN | C.EPOLLET)) == 0
	mut cs := state_for(mut w, server_fd)

	// First request, served over a consumed edge.
	req1 := 'GET /one HTTP/1.1\r\nHost: x\r\n\r\n'
	assert C.send(client_fd, req1.str, req1.len, C.MSG_NOSIGNAL) == req1.len
	mut event := C.epoll_event{}
	assert C.epoll_wait(epoll_fd, &event, 1, 1000) == 1
	assert unsafe { event.data.fd } == server_fd
	serve_conn(mut w, server_fd, mut cs)
	assert recv_available(client_fd).contains('HTTP/1.1 200 OK')
	// The connection stays alive and registered.
	assert unsafe { w.conns[server_fd] != nil }

	// A second request on the SAME connection fires a fresh readable edge.
	req2 := 'GET /two HTTP/1.1\r\nHost: x\r\n\r\n'
	assert C.send(client_fd, req2.str, req2.len, C.MSG_NOSIGNAL) == req2.len
	assert C.epoll_wait(epoll_fd, &event, 1, 1000) == 1
	assert unsafe { event.data.fd } == server_fd
	serve_conn(mut w, server_fd, mut cs)
	assert recv_available(client_fd).contains('HTTP/1.1 200 OK')

	C.close(server_fd)
	C.close(client_fd)
}

struct WorkerCounter {
mut:
	n int
}

fn test_worker_state_reaches_handler() ! {
	wc := &WorkerCounter{}
	make_state := fn [wc] () voidptr {
		return wc
	}
	handler := fn (req HttpRequest) !HttpResponse {
		mut c := unsafe { &WorkerCounter(req.worker_state) }
		c.n++
		return HttpResponse{
			content: 'HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()
		}
	}
	server := new_server(ServerConfig{
		family:     .ip
		port:       0
		handler:    handler
		make_state: make_state
	})!
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer { C.close(epoll_fd) }
	mut sockets := [2]int{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	server_fd := sockets[0]
	client_fd := sockets[1]
	set_blocking(server_fd, false)
	set_blocking(client_fd, false)

	mut w := new_regression_worker(server, epoll_fd)
	// Mirror what process_events does once per worker thread.
	w.worker_state = server.make_state()
	assert add_fd_to_epoll(epoll_fd, server_fd, u32(C.EPOLLIN | C.EPOLLET)) == 0
	mut cs := state_for(mut w, server_fd)

	req := 'GET /a HTTP/1.1\r\nHost: x\r\n\r\nGET /b HTTP/1.1\r\nHost: x\r\n\r\n'
	assert C.send(client_fd, req.str, req.len, C.MSG_NOSIGNAL) == req.len
	serve_conn(mut w, server_fd, mut cs)
	assert recv_available(client_fd).count('HTTP/1.1 200 OK') == 2
	// Both requests saw the SAME per-worker state instance.
	assert wc.n == 2

	C.close(server_fd)
	C.close(client_fd)
}

fn test_conn_state_is_pooled_with_buffers_retained() ! {
	server := new_server(ServerConfig{
		family:  .ip
		port:    0
		handler: regression_handler
	})!
	epoll_fd := C.epoll_create1(0)
	assert epoll_fd >= 0
	defer { C.close(epoll_fd) }
	mut sockets := [2]int{}
	assert C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) == 0
	server_fd := sockets[0]
	client_fd := sockets[1]
	set_blocking(server_fd, false)

	mut w := new_regression_worker(server, epoll_fd)
	assert add_fd_to_epoll(epoll_fd, server_fd, u32(C.EPOLLIN | C.EPOLLET)) == 0
	mut cs := state_for(mut w, server_fd)
	read_cap := cs.read_buf.cap
	write_cap := cs.write_buf.cap
	assert read_cap == read_buf_cap
	assert write_cap == write_buf_cap

	// Closing the connection retires its ConnState to the free-list (fd is closed
	// by close_conn), keeping the buffers.
	close_conn(mut w, server_fd)
	assert w.free_conns.len == 1
	assert unsafe { w.conns[server_fd] == nil }
	pooled := w.free_conns[0]
	assert pooled.read_buf.len == 0
	assert pooled.read_buf.cap == read_cap
	assert pooled.write_buf.cap == write_cap

	// The next connection reuses the pooled state (same buffers, no reallocation).
	reused := state_for(mut w, server_fd)
	assert w.free_conns.len == 0
	assert reused == pooled

	C.close(client_fd)
}
