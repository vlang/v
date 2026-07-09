module fasthttp

import net
import sync.stdatomic
import time

const epoll_wait_timeout_ms = 100
const status_408_response = 'HTTP/1.1 408 Request Timeout\r\nContent-Type: text/plain\r\nContent-Length: 19\r\nConnection: close\r\n\r\n408 Request Timeout'.bytes()

pub struct Server {
pub:
	family                  net.AddrFamily = .ip6
	port                    int            = 3000
	max_request_buffer_size int            = 8192
	timeout_in_seconds      int            = 30
	user_data               voidptr
mut:
	listen_fds      []int    = []int{len: max_thread_pool_size, cap: max_thread_pool_size, init: -1}
	epoll_fds       []int    = []int{len: max_thread_pool_size, cap: max_thread_pool_size, init: -1}
	threads         []thread = []thread{len: max_thread_pool_size, cap: max_thread_pool_size}
	request_handler fn (HttpRequest) !HttpResponse @[required]
	running         &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	shutting_down   &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	stopped         &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(true)
	active_requests &stdatomic.AtomicVal[int]  = stdatomic.new_atomic(0)
}

// new_server creates and initializes a new Server instance.
pub fn new_server(config ServerConfig) !&Server {
	if config.max_request_buffer_size <= 0 {
		return error('max_request_buffer_size must be greater than 0')
	}
	mut server := &Server{
		family:                  config.family
		port:                    config.port
		max_request_buffer_size: config.max_request_buffer_size
		timeout_in_seconds:      config.timeout_in_seconds
		user_data:               config.user_data
		request_handler:         config.handler
		running:                 stdatomic.new_atomic(false)
		shutting_down:           stdatomic.new_atomic(false)
		stopped:                 stdatomic.new_atomic(true)
		active_requests:         stdatomic.new_atomic(0)
	}
	unsafe {
		server.listen_fds.flags.set(.noslices | .noshrink | .nogrow)
		server.epoll_fds.flags.set(.noslices | .noshrink | .nogrow)
		server.threads.flags.set(.noslices | .noshrink | .nogrow)
	}
	return server
}

fn set_blocking(fd int, blocking bool) {
	flags := C.fcntl(fd, C.F_GETFL, 0)
	if flags == -1 {
		// TODO: better error handling
		eprintln(@LOCATION)
		return
	}
	if blocking {
		// This removes the O_NONBLOCK flag from flags and set it.
		C.fcntl(fd, C.F_SETFL, flags & ~C.O_NONBLOCK)
	} else {
		// This adds the O_NONBLOCK flag from flags and set it.
		C.fcntl(fd, C.F_SETFL, flags | C.O_NONBLOCK)
	}
}

// ClientWriteState carries the pending response bytes plus optional file data
// for a connection that could not be drained in a single non-blocking write.
// It lives in `client_write_states` until both the in-memory content and the
// file body have been fully transferred. While a state exists, the connection
// is also armed for EPOLLOUT so process_events can resume the write without
// blocking the worker.
struct ClientWriteState {
mut:
	content        []u8
	content_pos    int
	content_owned  bool
	file_fd        int = -1
	file_len       i64
	file_pos       i64
	should_close   bool
	request_arena  voidptr
	request_active bool
	start_ns       i64
	// epollout_armed records whether we actually had to register EPOLLOUT for
	// this connection. When set, complete_write performs a DEL+ADD on the fd to
	// re-deliver any pipelined request bytes that arrived during the write as a
	// fresh EPOLLIN edge (level-triggered semantics are not used here).
	epollout_armed bool
}

// drain_status describes the outcome of one try_drain_write() pass.
enum DrainStatus {
	done    // all bytes (content + file) have been sent
	blocked // the kernel send buffer is full; come back on EPOLLOUT
	failed  // unrecoverable error or peer closed mid-transfer
}

fn close_socket(fd int) bool {
	ret := C.close(fd)
	if ret == -1 {
		if C.errno == C.EINTR {
			// Interrupted by signal, retry is safe
			return close_socket(fd)
		}
		eprintln('ERROR: close(fd=${fd}) failed with errno=${C.errno}')
		return false
	}
	return true
}

fn create_server_socket(server Server) int {
	// Create a socket with non-blocking mode
	server_fd := C.socket(i32(server.family), i32(net.SocketType.tcp), 0)
	if server_fd < 0 {
		eprintln(@LOCATION)
		C.perror(c'Socket creation failed')
		return -1
	}

	set_blocking(server_fd, false)

	// Enable SO_REUSEADDR and SO_REUSEPORT
	opt := 1
	if C.setsockopt(server_fd, C.SOL_SOCKET, C.SO_REUSEADDR, &opt, sizeof(opt)) < 0 {
		eprintln(@LOCATION)
		C.perror(c'setsockopt SO_REUSEADDR failed')
		close_socket(server_fd)
		return -1
	}
	if C.setsockopt(server_fd, C.SOL_SOCKET, C.SO_REUSEPORT, &opt, sizeof(opt)) < 0 {
		eprintln(@LOCATION)
		C.perror(c'setsockopt SO_REUSEPORT failed')
		close_socket(server_fd)
		return -1
	}

	addr := if server.family == .ip6 {
		net.new_ip6(u16(server.port), [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]!)
	} else {
		net.new_ip(u16(server.port), [u8(0), 0, 0, 0]!)
	}
	alen := addr.len()
	if C.bind(server_fd, voidptr(&addr), alen) < 0 {
		eprintln(@LOCATION)
		C.perror(c'Bind failed')
		close_socket(server_fd)
		return -1
	}
	if C.listen(server_fd, max_connection_size) < 0 {
		eprintln(@LOCATION)
		C.perror(c'Listen failed')
		close_socket(server_fd)
		return -1
	}
	return server_fd
}

// add_fd_to_epoll adds a file descriptor to the epoll instance
fn add_fd_to_epoll(epoll_fd int, fd int, events u32) int {
	mut ev := C.epoll_event{
		events: events
	}
	ev.data.fd = fd
	if C.epoll_ctl(epoll_fd, C.EPOLL_CTL_ADD, fd, &ev) == -1 {
		eprintln(@LOCATION)
		C.perror(c'epoll_ctl')
		return -1
	}
	return 0
}

// remove_fd_from_epoll removes a file descriptor from the epoll instance
fn remove_fd_from_epoll(epoll_fd int, fd int) bool {
	ret := C.epoll_ctl(epoll_fd, C.EPOLL_CTL_DEL, fd, C.NULL)
	if ret == -1 {
		eprintln('ERROR: epoll_ctl(DEL, fd=${fd}) failed with errno=${C.errno}')
		return false
	}
	return true
}

fn handle_accept_loop(epoll_fd int, listen_fd int, mut client_fds map[int]bool) {
	for {
		client_fd := C.accept4(listen_fd, C.NULL, C.NULL, C.SOCK_NONBLOCK)
		if client_fd < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				break // No more incoming connections; exit loop.
			}
			eprintln(@LOCATION)
			C.perror(c'Accept failed')
			break
		}
		// Enable TCP_NODELAY for lower latency
		opt := 1
		C.setsockopt(client_fd, C.IPPROTO_TCP, C.TCP_NODELAY, &opt, sizeof(opt))
		// Register client socket with epoll
		if add_fd_to_epoll(epoll_fd, client_fd, u32(C.EPOLLIN | C.EPOLLET)) == -1 {
			close_socket(client_fd)
			continue
		}
		client_fds[client_fd] = true
	}
}

fn free_write_state(server &Server, client_fd int, mut client_write_states map[int]&ClientWriteState) {
	mut state := client_write_states[client_fd] or { return }
	if state.content_owned && state.content.cap > 0 {
		unsafe { state.content.free() }
	}
	state.content = []u8{}
	if state.file_fd != -1 {
		C.close(state.file_fd)
		state.file_fd = -1
	}
	$if prealloc {
		if state.request_arena != unsafe { nil } {
			unsafe { prealloc_scope_free_after(state.request_arena) }
		}
	}
	state.request_arena = unsafe { nil }
	if state.request_active {
		server.end_request()
		state.request_active = false
	}
	client_write_states.delete(client_fd)
	unsafe { free(state) }
}

fn handle_client_closure(server &Server, epoll_fd int, client_fd int, mut client_fds map[int]bool, mut client_buffers map[int][]u8, mut client_read_starts map[int]i64, mut closing_client_fds map[int]bool, mut client_write_states map[int]&ClientWriteState) {
	// Never close the listening socket here
	if client_fd == 0 {
		return
	}
	if client_fd <= 0 {
		eprintln('ERROR: Invalid FD=${client_fd} for closure')
		return
	}
	client_fds.delete(client_fd)
	client_buffers.delete(client_fd)
	client_read_starts.delete(client_fd)
	closing_client_fds.delete(client_fd)
	free_write_state(server, client_fd, mut client_write_states)
	remove_fd_from_epoll(epoll_fd, client_fd)
	close_socket(client_fd)
}

fn close_worker_clients(server &Server, epoll_fd int, mut client_fds map[int]bool, mut client_buffers map[int][]u8, mut client_read_starts map[int]i64, mut closing_client_fds map[int]bool, mut client_write_states map[int]&ClientWriteState) {
	for client_fd in client_fds.keys() {
		handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
			client_read_starts, mut closing_client_fds, mut client_write_states)
	}
}

fn drain_closing_client(server &Server, epoll_fd int, client_fd int, mut client_fds map[int]bool, mut client_buffers map[int][]u8, mut client_read_starts map[int]i64, mut closing_client_fds map[int]bool, mut client_write_states map[int]&ClientWriteState) {
	mut drain_buf := []u8{len: 4096}
	for {
		bytes_read := C.recv(client_fd, unsafe { &drain_buf[0] }, drain_buf.len, 0)
		if bytes_read < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				return
			}
			handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
			return
		}
		if bytes_read == 0 {
			handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
			return
		}
	}
}

fn send_terminal_response_and_drain(client_fd int, response []u8, mut client_buffers map[int][]u8, mut client_read_starts map[int]i64, mut closing_client_fds map[int]bool) {
	C.send(client_fd, response.data, response.len, C.MSG_NOSIGNAL)
	net.shutdown(client_fd, how: .write)
	client_buffers.delete(client_fd)
	client_read_starts.delete(client_fd)
	closing_client_fds[client_fd] = true
}

// try_drain_write attempts to push the remaining bytes of `state.content` and
// (if present) `state.file_fd` to the socket in non-blocking mode. It returns
// `.done` once all data has been transferred, `.blocked` when the kernel send
// buffer is full (EAGAIN/EWOULDBLOCK), or `.failed` on any unrecoverable error.
// The caller is responsible for parking the state until EPOLLOUT fires (when
// `.blocked` is returned) and for cleaning up via `free_write_state` once
// `.done` or `.failed` is reached.
fn try_drain_write(client_fd int, mut state ClientWriteState) DrainStatus {
	// 1. Drain in-memory content (response headers + possibly a body).
	for state.content_pos < state.content.len {
		remaining := state.content.len - state.content_pos
		sent := C.send(client_fd, unsafe { &state.content[state.content_pos] }, remaining,
			C.MSG_NOSIGNAL)
		if sent > 0 {
			state.content_pos += sent
			continue
		}
		if sent < 0 {
			errno_val := C.errno
			if errno_val == C.EAGAIN || errno_val == C.EWOULDBLOCK {
				return .blocked
			}
			if errno_val == C.EINTR {
				continue
			}
			eprintln('ERROR: send() failed with errno=${errno_val}')
			return .failed
		}
		// send() returning 0 on a non-blocking TCP socket is unusual; treat it as
		// a failure rather than spin.
		return .failed
	}

	// 2. Drain the optional file body via sendfile(2). sendfile updates the
	// offset pointer in place, so `state.file_pos` advances automatically.
	if state.file_fd != -1 {
		for state.file_pos < state.file_len {
			remaining := state.file_len - state.file_pos
			ssize := C.sendfile(client_fd, state.file_fd, &state.file_pos, usize(remaining))
			if ssize > 0 {
				continue
			}
			if ssize == 0 {
				// Short transfer: the file shrank between fstat() and now, or the
				// peer closed. We have already promised the full Content-Length,
				// so the response is now truncated -- closing the connection is
				// the only way to keep keep-alive clients in sync.
				eprintln('ERROR: sendfile() returned 0 with ${remaining} bytes still pending; closing connection to avoid desync')
				return .failed
			}
			errno_val := C.errno
			if errno_val == C.EAGAIN || errno_val == C.EWOULDBLOCK {
				return .blocked
			}
			if errno_val == C.EINTR {
				continue
			}
			match errno_val {
				C.EBADF {
					eprintln('ERROR: sendfile() EBADF: input fd or socket not open for required access (errno=${errno_val})')
				}
				C.EFAULT {
					eprintln('ERROR: sendfile() EFAULT: bad address for offset (errno=${errno_val})')
				}
				C.EINVAL {
					eprintln('ERROR: sendfile() EINVAL: invalid descriptor state or non-seekable input (errno=${errno_val})')
				}
				C.EIO {
					eprintln('ERROR: sendfile() EIO: I/O error while reading input file (errno=${errno_val})')
				}
				C.ENOMEM {
					eprintln('ERROR: sendfile() ENOMEM: insufficient kernel memory (errno=${errno_val})')
				}
				C.EOVERFLOW {
					eprintln('ERROR: sendfile() EOVERFLOW: count exceeds file/socket limits (errno=${errno_val})')
				}
				C.ESPIPE {
					eprintln('ERROR: sendfile() ESPIPE: input file not seekable with offset (errno=${errno_val})')
				}
				else {
					eprintln('ERROR: sendfile() failed with errno=${errno_val}')
				}
			}

			return .failed
		}
		// Done with the file -- close the fd eagerly so keep-alive doesn't hold it open.
		C.close(state.file_fd)
		state.file_fd = -1
	}

	return .done
}

// arm_epollout switches the connection's epoll mask from EPOLLIN|EPOLLET to
// EPOLLIN|EPOLLOUT|EPOLLET so the next round of pending bytes is delivered as
// an EPOLLOUT event instead of blocking the worker.
fn arm_epollout(epoll_fd int, client_fd int) int {
	mut ev := C.epoll_event{
		events: u32(C.EPOLLIN | C.EPOLLOUT | C.EPOLLET)
	}
	ev.data.fd = client_fd
	return C.epoll_ctl(epoll_fd, C.EPOLL_CTL_MOD, client_fd, &ev)
}

// complete_write tears down the per-fd write state after a successful drain,
// then either closes the connection or leaves it in keep-alive mode.
fn complete_write(server &Server, epoll_fd int, client_fd int, mut client_fds map[int]bool, mut client_buffers map[int][]u8, mut client_read_starts map[int]i64, mut closing_client_fds map[int]bool, mut client_write_states map[int]&ClientWriteState) {
	state := client_write_states[client_fd] or { return }
	should_close := state.should_close
	free_write_state(server, client_fd, mut client_write_states)
	client_buffers.delete(client_fd)
	if server.is_shutting_down() || should_close {
		handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
			client_read_starts, mut closing_client_fds, mut client_write_states)
		return
	}
	// Keep-alive: drop the fd from epoll and re-add it with the original
	// EPOLLIN|EPOLLET mask. The re-add causes the kernel to fire a fresh edge
	// for any pipelined request bytes that piled up in the recv buffer while the
	// response was being written; a plain EPOLL_CTL_MOD would not generate that
	// edge. Do this even when EPOLLOUT was never armed, because the inline write
	// path can also consume the current EPOLLIN edge before pipelined bytes are
	// observed by the loop again.
	remove_fd_from_epoll(epoll_fd, client_fd)
	if add_fd_to_epoll(epoll_fd, client_fd, u32(C.EPOLLIN | C.EPOLLET)) == -1 {
		handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
			client_read_starts, mut closing_client_fds, mut client_write_states)
	}
}

// handle_write resumes a blocked write when EPOLLOUT fires for `client_fd`.
fn handle_write(server &Server, epoll_fd int, client_fd int, mut client_fds map[int]bool, mut client_buffers map[int][]u8, mut client_read_starts map[int]i64, mut closing_client_fds map[int]bool, mut client_write_states map[int]&ClientWriteState) {
	mut state := client_write_states[client_fd] or { return }
	status := try_drain_write(client_fd, mut state)
	match status {
		.done {
			complete_write(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
		}
		.blocked {
			// Still blocked -- stay armed for the next EPOLLOUT.
		}
		.failed {
			handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
		}
	}
}

fn process_request(server &Server, epoll_fd int, client_fd int, request_buffer []u8, mut client_fds map[int]bool, mut client_buffers map[int][]u8, mut client_read_starts map[int]i64, mut closing_client_fds map[int]bool, mut client_write_states map[int]&ClientWriteState) {
	mut request_arena := voidptr(unsafe { nil })
	$if prealloc {
		request_arena = unsafe { prealloc_scope_begin() }
	}
	client_read_starts.delete(client_fd)
	server.begin_request()
	mut request_active := true

	mut decoded_http_request := decode_http_request(request_buffer) or {
		eprintln('Error decoding request ${err}')
		C.send(client_fd, tiny_bad_request_response.data, tiny_bad_request_response.len,
			C.MSG_NOSIGNAL)
		end_request_arena_current_thread(request_arena)
		if request_active {
			server.end_request()
		}
		handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
			client_read_starts, mut closing_client_fds, mut client_write_states)
		return
	}
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'fasthttp decoded request') }
	}
	decoded_http_request.client_conn_fd = client_fd
	decoded_http_request.client_conn_handle = usize(client_fd)
	decoded_http_request.user_data = server.user_data
	mut response := server.request_handler(decoded_http_request) or {
		eprintln('Error handling request ${err}')
		C.send(client_fd, tiny_bad_request_response.data, tiny_bad_request_response.len,
			C.MSG_NOSIGNAL)
		end_request_arena_current_thread(request_arena)
		if request_active {
			server.end_request()
		}
		handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
			client_read_starts, mut closing_client_fds, mut client_write_states)
		return
	}
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'fasthttp handler returned') }
	}
	response.attach_request_arena_if_empty(request_arena)

	match response.takeover_mode {
		.manual {
			// The handler has taken ownership of the connection.
			// Remove from epoll and tracking before ending the request, but do
			// NOT close the fd.
			client_fds.delete(client_fd)
			client_buffers.delete(client_fd)
			client_read_starts.delete(client_fd)
			closing_client_fds.delete(client_fd)
			remove_fd_from_epoll(epoll_fd, client_fd)
			response.abandon_request_arena_current_thread()
			response.free_owned_content()
			if request_active {
				server.end_request()
				request_active = false
			}
			return
		}
		.reusable {
			set_blocking(client_fd, false)
			client_buffers.delete(client_fd)
			response.free_owned_content()
			response.end_request_arena_current_thread()
			if request_active {
				server.end_request()
			}
			if server.is_shutting_down() || response.should_close {
				handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut
					client_buffers, mut client_read_starts, mut closing_client_fds, mut
					client_write_states)
			}
			return
		}
		.none {}
	}

	// Open the file (if any) before constructing the state so we can fail fast
	// while we still own the request arena via response.
	mut file_fd := -1
	mut file_len := i64(0)
	if response.file_path != '' {
		fd := C.open(response.file_path.str, C.O_RDONLY, 0)
		if fd == -1 {
			eprintln('ERROR: open file failed')
			response.free_owned_content()
			response.end_request_arena_current_thread()
			if request_active {
				server.end_request()
			}
			handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
			return
		}
		mut st := C.stat{}
		if C.fstat(fd, &st) != 0 {
			eprintln('ERROR: fstat failed')
			C.close(fd)
			response.free_owned_content()
			response.end_request_arena_current_thread()
			if request_active {
				server.end_request()
			}
			handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
			return
		}
		file_fd = fd
		file_len = i64(st.st_size)
	}

	// Move content + arena ownership into the per-fd state. After this point we
	// must not touch the response's arena via end/free (it will be released by
	// free_write_state once the drain completes).
	content_bytes := response.take_or_clone_content()
	arena_ptr := response.take_request_arena()
	leave_request_arena_current_thread(arena_ptr)

	mut state := &ClientWriteState{
		content:        content_bytes
		content_pos:    0
		content_owned:  true
		file_fd:        file_fd
		file_len:       file_len
		file_pos:       0
		should_close:   response.should_close
		request_arena:  arena_ptr
		request_active: request_active
		start_ns:       time.sys_mono_now()
	}
	// From here on, the state owns end_request bookkeeping.
	request_active = false
	client_write_states[client_fd] = state

	status := try_drain_write(client_fd, mut state)
	match status {
		.done {
			complete_write(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
		}
		.blocked {
			// Kernel send buffer is full. Park the state and resume on EPOLLOUT.
			client_buffers.delete(client_fd)
			client_read_starts.delete(client_fd)
			if arm_epollout(epoll_fd, client_fd) == -1 {
				eprintln('ERROR: epoll_ctl(MOD, EPOLLOUT) failed errno=${C.errno}')
				handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut
					client_buffers, mut client_read_starts, mut closing_client_fds, mut
					client_write_states)
			} else {
				state.epollout_armed = true
			}
		}
		.failed {
			handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
		}
	}
}

fn process_events(server &Server, epoll_fd int, listen_fd int) {
	mut events := [max_connection_size]C.epoll_event{}
	mut request_buffer := []u8{len: server.max_request_buffer_size, cap: server.max_request_buffer_size}
	mut client_fds := map[int]bool{}
	mut client_buffers := map[int][]u8{}
	mut client_read_starts := map[int]i64{}
	mut closing_client_fds := map[int]bool{}
	mut client_write_states := map[int]&ClientWriteState{}
	unsafe {
		request_buffer.flags.set(.noslices | .nogrow | .noshrink)
	}
	for {
		if server.is_shutting_down() && server.active_request_count() == 0 {
			close_worker_clients(server, epoll_fd, mut client_fds, mut client_buffers, mut
				client_read_starts, mut closing_client_fds, mut client_write_states)
			return
		}
		num_events := C.epoll_wait(epoll_fd, &events[0], max_connection_size, epoll_wait_timeout_ms)
		if num_events < 0 {
			if C.errno == C.EINTR {
				continue
			}
			if server.is_shutting_down() {
				continue
			}
			eprintln('ERROR: epoll_wait() failed with errno=${C.errno}')
			continue
		}
		for i := 0; i < num_events; i++ {
			client_fd := unsafe { events[i].data.fd }
			// Accept new connections when the listening socket is readable
			if client_fd == listen_fd {
				if server.is_shutting_down() {
					continue
				}
				handle_accept_loop(epoll_fd, listen_fd, mut client_fds)
				continue
			}

			if events[i].events & u32((C.EPOLLHUP | C.EPOLLERR)) != 0 {
				if client_fd == listen_fd {
					eprintln('ERROR: listen fd had HUP/ERR')
					continue
				}
				if client_fd > 0 {
					// Don't bother sending 444 if there is an in-flight write -- the
					// peer is already disconnected and we want to tear down promptly.
					if client_fd !in client_write_states {
						C.send(client_fd, status_444_response.data, status_444_response.len,
							C.MSG_NOSIGNAL)
					}
					handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut
						client_buffers, mut client_read_starts, mut closing_client_fds, mut
						client_write_states)
				} else {
					eprintln('ERROR: Invalid FD from epoll: ${client_fd}')
				}
				continue
			}
			if events[i].events & u32(C.EPOLLOUT) != 0 {
				handle_write(server, epoll_fd, client_fd, mut client_fds, mut client_buffers, mut
					client_read_starts, mut closing_client_fds, mut client_write_states)
				// If both EPOLLIN and EPOLLOUT fired, we still want to drain the
				// socket -- fall through to the EPOLLIN branch below.
				if client_fd !in client_fds {
					continue
				}
			}
			if events[i].events & u32(C.EPOLLIN) != 0 {
				if server.is_shutting_down() {
					handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut
						client_buffers, mut client_read_starts, mut closing_client_fds, mut
						client_write_states)
					continue
				}
				if closing_client_fds[client_fd] or { false } {
					drain_closing_client(server, epoll_fd, client_fd, mut client_fds, mut
						client_buffers, mut client_read_starts, mut closing_client_fds, mut
						client_write_states)
					continue
				}
				if client_fd in client_write_states {
					// A response is still being drained for this fd. Reading a new
					// request now would overwrite the in-flight ClientWriteState and
					// silently drop the response/file transfer. Defer until the write
					// completes -- on keep-alive completion we re-arm EPOLLIN below so
					// any bytes that arrived during the write get delivered as a fresh
					// edge.
					continue
				}
				// Read all available data from the socket
				mut total_bytes_read := 0
				mut readed_request_buffer := client_buffers[client_fd] or {
					[]u8{cap: server.max_request_buffer_size}
				}
				mut headers_complete := false
				mut header_too_large := false
				mut header_end_pos := -1
				mut request_complete := false
				mut peer_closed := false
				mut recv_error := false

				for {
					bytes_read := C.recv(client_fd, unsafe { &request_buffer[0] },
						server.max_request_buffer_size - 1, 0)
					if bytes_read < 0 {
						if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
							// No more data available right now
							break
						}
						// Error occurred
						eprintln('ERROR: recv() failed with errno=${C.errno}')
						recv_error = true
						break
					} else if bytes_read == 0 {
						// Connection closed by client
						peer_closed = true
						break
					}

					unsafe {
						readed_request_buffer.push_many(&request_buffer[0], bytes_read)
					}
					total_bytes_read += bytes_read
					if client_fd !in client_read_starts {
						client_read_starts[client_fd] = time.sys_mono_now()
					}

					// Enforce the configured limit on request headers, not on the whole body.
					buffer_len := readed_request_buffer.len
					if !headers_complete && buffer_len >= 4 {
						header_end_pos = find_header_end_in_buf(readed_request_buffer.data,
							buffer_len)
						if header_end_pos == -1 {
							if buffer_len >= server.max_request_buffer_size {
								header_too_large = true
								break
							}
						} else {
							headers_complete = true
							if header_end_pos > server.max_request_buffer_size {
								header_too_large = true
								break
							}
						}
					}

					if headers_complete && has_complete_body(readed_request_buffer.data, buffer_len) {
						request_complete = true
						break
					}
				}

				if header_too_large {
					send_terminal_response_and_drain(client_fd, status_413_response, mut
						client_buffers, mut client_read_starts, mut closing_client_fds)
					continue
				}
				if request_complete {
					process_request(server, epoll_fd, client_fd, readed_request_buffer, mut
						client_fds, mut client_buffers, mut client_read_starts, mut
						closing_client_fds, mut client_write_states)
				} else if recv_error {
					// Unexpected recv error - send 444 No Response
					C.send(client_fd, status_444_response.data, status_444_response.len,
						C.MSG_NOSIGNAL)
					handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut
						client_buffers, mut client_read_starts, mut closing_client_fds, mut
						client_write_states)
				} else if peer_closed || (total_bytes_read == 0 && readed_request_buffer.len == 0) {
					// Normal client closure (FIN received)
					handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut
						client_buffers, mut client_read_starts, mut closing_client_fds, mut
						client_write_states)
				} else if readed_request_buffer.len > 0 {
					client_buffers[client_fd] = readed_request_buffer
				}
			}
		}
		if server.timeout_in_seconds > 0 {
			now := time.sys_mono_now()
			timeout_ns := i64(server.timeout_in_seconds) * 1_000_000_000
			for client_fd in client_read_starts.keys() {
				started := client_read_starts[client_fd] or { continue }
				if now - started >= timeout_ns {
					send_terminal_response_and_drain(client_fd, status_408_response, mut
						client_buffers, mut client_read_starts, mut closing_client_fds)
				}
			}
			// Sweep write-side stalls: a client that armed EPOLLOUT but never
			// drains within `timeout_in_seconds` gets the connection torn down so
			// the worker isn't pinned waiting for a dead peer.
			for client_fd in client_write_states.keys() {
				state := client_write_states[client_fd] or { continue }
				if state.start_ns > 0 && now - state.start_ns >= timeout_ns {
					handle_client_closure(server, epoll_fd, client_fd, mut client_fds, mut
						client_buffers, mut client_read_starts, mut closing_client_fds, mut
						client_write_states)
				}
			}
		}
	}
}

fn (mut server Server) stop_accepting() {
	for i := 0; i < max_thread_pool_size; i++ {
		if server.listen_fds[i] < 0 {
			continue
		}
		if server.epoll_fds[i] >= 0 {
			remove_fd_from_epoll(server.epoll_fds[i], server.listen_fds[i])
		}
		close_socket(server.listen_fds[i])
		server.listen_fds[i] = -1
	}
}

// run starts the server and begins listening for incoming connections.
pub fn (mut server Server) run() ! {
	$if windows {
		eprintln('Windows is not supported yet')
		return
	}
	for i := 0; i < max_thread_pool_size; i++ {
		server.listen_fds[i] = create_server_socket(server)
		if server.listen_fds[i] < 0 {
			return
		}

		server.epoll_fds[i] = C.epoll_create1(0)
		if server.epoll_fds[i] < 0 {
			C.perror(c'epoll_create1 failed')
			close_socket(server.listen_fds[i])
			return
		}

		// Register the listening socket with each worker epoll for distributed accepts (edge-triggered)
		if add_fd_to_epoll(server.epoll_fds[i], server.listen_fds[i], u32(C.EPOLLIN | C.EPOLLET)) == -1 {
			close_socket(server.listen_fds[i])
			close_socket(server.epoll_fds[i])
			return
		}

		server.threads[i] = spawn process_events(server, server.epoll_fds[i], server.listen_fds[i])
	}

	server.mark_running()
	println('listening on http://0.0.0.0:${server.port}/')
	// Main thread waits for workers; accepts are handled in worker epoll loops
	for i in 0 .. max_thread_pool_size {
		server.threads[i].wait()
		if server.epoll_fds[i] >= 0 {
			close_socket(server.epoll_fds[i])
			server.epoll_fds[i] = -1
		}
	}
	server.mark_stopped()
}
