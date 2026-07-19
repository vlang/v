module fasthttp

import net
import sync.stdatomic
import time

const epoll_wait_timeout_ms = 100
const status_408_response = 'HTTP/1.1 408 Request Timeout\r\nContent-Type: text/plain\r\nContent-Length: 19\r\nConnection: close\r\n\r\n408 Request Timeout'.bytes()

// Per-connection buffer capacities. Both buffers are allocated once per connection
// and reused for its whole lifetime — capacity is kept across requests, so the
// steady state does zero per-request buffer allocation.
const read_buf_cap = 8 * 1024
const write_buf_cap = 16 * 1024
// Initial size of the flat fd-indexed connection table; it doubles on demand.
const conn_table_min = 1024
// Bound a single sendfile(2) call so one connection can't monopolize the worker;
// the remainder streams on the next writable edge.
const sendfile_chunk = 1024 * 1024
// Write-side cap: close a peer that pipelines requests but never drains responses
// (otherwise the per-connection write buffer would grow without bound).
const max_pending_write = 8 * 1024 * 1024

@[heap]
pub struct Server {
pub:
	family                  net.AddrFamily = .ip6
	port                    int            = 3000
	max_request_buffer_size int            = 8192
	timeout_in_seconds      int            = 30
	user_data               voidptr
mut:
	listen_fds      []int                          = []int{len: max_thread_pool_size, cap: max_thread_pool_size, init: -1}
	epoll_fds       []int                          = []int{len: max_thread_pool_size, cap: max_thread_pool_size, init: -1}
	threads         []thread                       = []thread{len: max_thread_pool_size, cap: max_thread_pool_size}
	request_handler fn (HttpRequest) !HttpResponse = unsafe { nil }
	append_handler  AppendHandler                  = unsafe { nil }
	make_state      fn () voidptr                  = unsafe { nil }
	running         &stdatomic.AtomicVal[bool]     = stdatomic.new_atomic(false)
	shutting_down   &stdatomic.AtomicVal[bool]     = stdatomic.new_atomic(false)
	stopped         &stdatomic.AtomicVal[bool]     = stdatomic.new_atomic(true)
	active_requests &stdatomic.AtomicVal[int]      = stdatomic.new_atomic(0)
}

// new_server creates and initializes a new Server instance.
pub fn new_server(config ServerConfig) !&Server {
	if config.max_request_buffer_size <= 0 {
		return error('max_request_buffer_size must be greater than 0')
	}
	has_handler := config.handler != unsafe { nil }
	has_append := config.append_handler != unsafe { nil }
	if !has_handler && !has_append {
		return error('a handler is required: set exactly one of `handler` or `append_handler`')
	}
	if has_handler && has_append {
		return error('set only one of `handler` or `append_handler`, not both')
	}
	mut server := &Server{
		family:                  config.family
		port:                    config.port
		max_request_buffer_size: config.max_request_buffer_size
		timeout_in_seconds:      config.timeout_in_seconds
		user_data:               config.user_data
		request_handler:         config.handler
		append_handler:          config.append_handler
		make_state:              config.make_state
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

// ConnState is the persistent per-connection state. It is created once (on the
// connection's first event), pooled on close, and reused for the whole
// connection lifetime. The buffers keep their capacity across requests:
// `read_buf` accumulates request bytes across edge-triggered reads, `write_buf`
// accumulates the responses to every pipelined request in one burst and is sent
// in a single write. No per-request allocation and no per-request free.
struct ConnState {
mut:
	read_buf  []u8 // buffered request bytes; len = bytes not yet consumed
	write_buf []u8 // buffered response bytes; [write_off..len) still pending
	write_off int
	// Deferred file body streamed with sendfile(2) AFTER write_buf drains (a
	// handler returned a file_path response). file_fd is OWNED here and closed
	// once fully sent; file_off is advanced by the kernel as bytes go out.
	file_fd        int = -1
	file_off       i64
	file_remaining i64
	should_close   bool // close the connection once the current batch is flushed
	request_active bool // a response is buffered/parked and counts toward active_requests
	read_start_ns  i64  // monotonic ns; >0 while a partial request is buffered (408)
	write_start_ns i64  // monotonic ns; >0 while a batch is parked for writing
	// request_arena is the -prealloc scope that must be freed once a parked write
	// completes (the response bytes were copied out of it into write_buf, but the
	// scope is kept and freed as a unit for symmetry with the non-parked path).
	request_arena voidptr
}

// Worker is a single epoll thread's local state: the flat fd-indexed connection
// table plus the free-list of retired ConnStates (each keeping its buffers).
struct Worker {
mut:
	server       &Server = unsafe { nil }
	epoll_fd     int
	listen_fd    int
	conns        []&ConnState
	free_conns   []&ConnState
	parked       int     // connections with an armed read/write deadline (gates the sweep)
	worker_state voidptr // this worker thread's ServerConfig.make_state value (nil if unset)
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

fn create_server_socket(server &Server) int {
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

// mod_fd_in_epoll changes the event mask an fd is registered for.
fn mod_fd_in_epoll(epoll_fd int, fd int, events u32) int {
	mut ev := C.epoll_event{
		events: events
	}
	ev.data.fd = fd
	return C.epoll_ctl(epoll_fd, C.EPOLL_CTL_MOD, fd, &ev)
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

// state_for returns the ConnState for fd, creating it (with its persistent
// buffers) on first use or reusing a pooled one. The table grows by doubling, so
// fd indexing stays O(1) with no hashing.
@[direct_array_access]
fn state_for(mut w Worker, fd int) &ConnState {
	if fd >= w.conns.len {
		mut new_len := w.conns.len
		for new_len <= fd {
			new_len *= 2
		}
		mut grown := []&ConnState{len: new_len, init: unsafe { nil }}
		for i in 0 .. w.conns.len {
			grown[i] = w.conns[i]
		}
		w.conns = grown
	}
	if unsafe { w.conns[fd] == nil } {
		if w.free_conns.len > 0 {
			w.conns[fd] = w.free_conns.pop()
			return w.conns[fd]
		}
		w.conns[fd] = &ConnState{
			read_buf:  []u8{len: 0, cap: read_buf_cap}
			write_buf: []u8{len: 0, cap: write_buf_cap}
		}
	}
	return w.conns[fd]
}

// mark_active counts a connection's in-flight response toward active_requests
// (once) so a graceful shutdown drains it before closing.
@[inline]
fn mark_active(mut w Worker, mut cs ConnState) {
	if !cs.request_active {
		w.server.begin_request()
		cs.request_active = true
	}
}

@[inline]
fn clear_active(mut w Worker, mut cs ConnState) {
	if cs.request_active {
		w.server.end_request()
		cs.request_active = false
	}
}

// close_conn removes fd from epoll, closes it, and returns its ConnState to the
// per-worker free-list with its buffers retained (length reset, capacity kept).
// Every field a fresh ConnState would have is reset so no stale state bleeds into
// the next connection that reuses this slot.
@[direct_array_access]
fn close_conn(mut w Worker, fd int) {
	if fd <= 0 {
		return
	}
	if fd < w.conns.len {
		mut cs := w.conns[fd]
		if unsafe { cs != nil } {
			clear_active(mut w, mut cs)
			// A connection can have BOTH deadlines armed (leftover read bytes while a
			// write is parked); decrement once per armed deadline so w.parked stays exact.
			if cs.read_start_ns != 0 {
				w.parked--
			}
			if cs.write_start_ns != 0 {
				w.parked--
			}
			if cs.file_fd != -1 {
				C.close(cs.file_fd)
			}
			$if prealloc {
				if cs.request_arena != unsafe { nil } {
					unsafe { prealloc_scope_free_after(cs.request_arena) }
				}
			}
			unsafe {
				cs.read_buf.len = 0
				cs.write_buf.len = 0
			}
			cs.write_off = 0
			cs.file_fd = -1
			cs.file_off = 0
			cs.file_remaining = 0
			cs.should_close = false
			cs.read_start_ns = 0
			cs.write_start_ns = 0
			cs.request_arena = unsafe { nil }
			w.conns[fd] = unsafe { nil }
			w.free_conns << cs
		}
	}
	remove_fd_from_epoll(w.epoll_fd, fd)
	close_socket(fd)
}

// detach_conn is like close_conn but does NOT close the fd or remove it from
// epoll's DEL is still issued — used for `.manual` takeover where the handler now
// owns the connection. The ConnState is pooled; the fd is left open.
@[direct_array_access]
fn detach_conn(mut w Worker, fd int) {
	if fd <= 0 || fd >= w.conns.len {
		return
	}
	mut cs := w.conns[fd]
	if unsafe { cs == nil } {
		return
	}
	clear_active(mut w, mut cs)
	if cs.read_start_ns != 0 {
		w.parked--
	}
	if cs.write_start_ns != 0 {
		w.parked--
	}
	// A takeover handler owns the connection and its allocation lifetime; abandon
	// the request arena rather than freeing it under the handler.
	$if prealloc {
		if cs.request_arena != unsafe { nil } {
			abandon_request_arena_current_thread(cs.request_arena)
		}
	}
	unsafe {
		cs.read_buf.len = 0
		cs.write_buf.len = 0
	}
	cs.write_off = 0
	cs.file_fd = -1
	cs.file_off = 0
	cs.file_remaining = 0
	cs.should_close = false
	cs.request_arena = unsafe { nil }
	w.conns[fd] = unsafe { nil }
	w.free_conns << cs
	remove_fd_from_epoll(w.epoll_fd, fd)
}

// mark_read_deadline arms/clears the read deadline for a partially-read request.
@[inline]
fn mark_read_deadline(mut w Worker, mut cs ConnState) {
	if cs.read_buf.len > 0 {
		if w.server.timeout_in_seconds > 0 && cs.read_start_ns == 0 {
			cs.read_start_ns = time.sys_mono_now()
			w.parked++
		}
	} else if cs.read_start_ns != 0 {
		cs.read_start_ns = 0
		w.parked--
	}
}

// drain_file streams the deferred file body with sendfile(2), advancing
// file_off/file_remaining. Returns 1 (fully sent), 0 (blocked — resume on the
// next writable edge) or -1 (hard error — close the connection).
fn drain_file(fd int, mut cs ConnState) int {
	for cs.file_remaining > 0 {
		want := if cs.file_remaining > sendfile_chunk {
			usize(sendfile_chunk)
		} else {
			usize(cs.file_remaining)
		}
		sent := C.sendfile(fd, cs.file_fd, &cs.file_off, want)
		if sent > 0 {
			cs.file_remaining -= i64(sent)
			continue
		}
		if sent < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				return 0
			}
			if C.errno == C.EINTR {
				continue
			}
			return -1
		}
		// sent == 0: the file shrank between fstat() and now, or the peer closed.
		// The promised Content-Length can no longer be met — close to resync.
		return -1
	}
	return 1
}

// flush_batch sends all pending response bytes then streams any deferred file
// body, or parks the remainder for EPOLLOUT. On full completion the write buffer
// is reset (capacity kept) and the connection is either closed or kept alive.
// Returns false if the connection was closed (the caller must not touch it).
fn flush_batch(mut w Worker, fd int, mut cs ConnState) bool {
	// Phase 1: the buffered response bytes (status lines, headers, small bodies).
	for cs.write_off < cs.write_buf.len {
		n := C.send(fd, unsafe { &u8(cs.write_buf.data) + cs.write_off },
			usize(cs.write_buf.len - cs.write_off), C.MSG_NOSIGNAL)
		if n > 0 {
			cs.write_off += n
			continue
		}
		if n < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				return park_write(mut w, fd, mut cs)
			}
			if C.errno == C.EINTR {
				continue
			}
		}
		close_conn(mut w, fd)
		return false
	}
	// Phase 2: stream the deferred file body straight from the page cache. Guard on
	// file_fd, not file_remaining: a zero-length file leaves an open fd with
	// file_remaining == 0, and it must still be closed here (drain_file returns 1
	// immediately for it) or the descriptor leaks across a keep-alive connection.
	if cs.file_fd != -1 {
		match drain_file(fd, mut cs) {
			0 {
				return park_write(mut w, fd, mut cs)
			}
			-1 {
				close_conn(mut w, fd)
				return false
			}
			else {
				C.close(cs.file_fd)
				cs.file_fd = -1
			}
		}
	}
	// Everything sent. Recycle the write buffer and settle the connection.
	unsafe {
		cs.write_buf.len = 0
	}
	cs.write_off = 0
	if cs.write_start_ns != 0 {
		cs.write_start_ns = 0
		w.parked--
	}
	$if prealloc {
		if cs.request_arena != unsafe { nil } {
			unsafe { prealloc_scope_free_after(cs.request_arena) }
			cs.request_arena = unsafe { nil }
		}
	}
	clear_active(mut w, mut cs)
	if w.server.is_shutting_down() || cs.should_close {
		close_conn(mut w, fd)
		return false
	}
	// Keep-alive: make sure the connection is armed only for readability again.
	mod_fd_in_epoll(w.epoll_fd, fd, u32(C.EPOLLIN | C.EPOLLET))
	return true
}

// park_write arms the write deadline (once), subscribes the fd to EPOLLOUT so the
// unfinished batch resumes on the next writable edge, and keeps the response
// counted as in-flight. Always returns true (still alive, just parked).
fn park_write(mut w Worker, fd int, mut cs ConnState) bool {
	if w.server.timeout_in_seconds > 0 && cs.write_start_ns == 0 {
		cs.write_start_ns = time.sys_mono_now()
		w.parked++
	}
	mark_active(mut w, mut cs)
	mod_fd_in_epoll(w.epoll_fd, fd, u32(C.EPOLLIN | C.EPOLLOUT | C.EPOLLET))
	return true
}

// compact_read_buf drops the first `pos` consumed bytes, keeping any leftover
// (partial / not-yet-framed) request at the front of the buffer.
@[direct_array_access; inline]
fn compact_read_buf(mut cs ConnState, pos int) {
	if pos <= 0 {
		return
	}
	leftover := cs.read_buf.len - pos
	if leftover > 0 {
		unsafe { C.memmove(cs.read_buf.data, &u8(cs.read_buf.data) + pos, usize(leftover)) }
	}
	unsafe {
		cs.read_buf.len = leftover
	}
}

// open_deferred_file opens response.file_path for a sendfile(2) body streamed by
// flush_batch after the buffered bytes, or marks the connection to close on error.
fn open_deferred_file(mut cs ConnState, file_path string) {
	file_fd := C.open(file_path.str, C.O_RDONLY, 0)
	if file_fd == -1 {
		eprintln('ERROR: open file failed: ${file_path}')
		cs.should_close = true
		return
	}
	mut st := C.stat{}
	if C.fstat(file_fd, &st) != 0 {
		eprintln('ERROR: fstat failed: ${file_path}')
		C.close(file_fd)
		cs.should_close = true
		return
	}
	cs.file_fd = file_fd
	cs.file_off = 0
	cs.file_remaining = i64(st.st_size)
}

// drain_requests answers every complete request currently buffered in read_buf,
// appending each response to write_buf so the whole burst goes out in one send.
// Returns false if the connection was closed (the caller must not touch it).
@[direct_array_access]
fn drain_requests(mut w Worker, fd int, mut cs ConnState) bool {
	mut pos := 0
	// max_header bounds the request-head size (→ 413 before the handler runs); the
	// body is left unbounded here to preserve the legacy no-body-limit behavior.
	max_header := w.server.max_request_buffer_size
	for pos < cs.read_buf.len {
		total := frame_request_length_lim_idx(buf_view(cs.read_buf, pos, cs.read_buf.len - pos),
			max_header, 0)
		if total == -1 {
			break // incomplete — wait for more bytes
		}
		if total < -1 {
			// Malformed framing (413/431/400 sentinels). Answer once and close.
			cs.write_buf << if total == frame_err_body || total == frame_err_header {
				status_413_response
			} else {
				tiny_bad_request_response
			}
			pos += cs.read_buf.len - pos // consume the rest; we are closing
			cs.should_close = true
			compact_read_buf(mut cs, pos)
			mark_active(mut w, mut cs)
			return flush_batch(mut w, fd, mut cs)
		}
		// A file deferred by an earlier request in this batch must be streamed (in
		// byte order) before this next response can be appended: flush now. Gate on
		// file_fd (not file_remaining) so a zero-length deferred file is drained and
		// its fd closed here instead of being orphaned by the next open_deferred_file.
		if cs.file_fd != -1 {
			compact_read_buf(mut cs, pos)
			mark_active(mut w, mut cs)
			if !flush_batch(mut w, fd, mut cs) {
				return false
			}
			pos = 0
		}

		req_view := buf_view(cs.read_buf, pos, total)
		pos += total

		if w.server.append_handler != unsafe { nil } {
			// Zero-copy path: the handler appends its response directly into the
			// reused write buffer. The reactor opens NO -prealloc scope here (growing
			// write_buf inside a scope would free it out from under us); a handler
			// that wants request-scoped arenas manages its own and must leave it
			// before writing into `out`.
			mut decoded := decode_http_request(req_view) or {
				eprintln('Error decoding request ${err}')
				cs.write_buf << tiny_bad_request_response
				cs.should_close = true
				compact_read_buf(mut cs, pos)
				mark_active(mut w, mut cs)
				return flush_batch(mut w, fd, mut cs)
			}
			decoded.client_conn_fd = fd
			decoded.client_conn_handle = usize(fd)
			decoded.user_data = w.server.user_data
			decoded.worker_state = w.worker_state
			mut ctl := ResponseControl{}
			// write_len before the handler runs: a takeover handler writes its
			// response directly to the socket (it does not append to `out`), so any
			// growth here for .manual/.reusable is earlier pipelined responses that
			// were already buffered — those must not be reordered behind the direct
			// write. (Snapshot before the call because the handler may grow write_buf.)
			pre_write_len := cs.write_buf.len
			step := w.server.append_handler(decoded, mut cs.write_buf, w.worker_state, mut ctl)
			match ctl.takeover_mode {
				.manual, .reusable {
					// A takeover handler already wrote its response straight to the socket.
					// If earlier pipelined responses are still buffered, flushing them now
					// would put them AFTER the takeover write on the wire — reversing
					// HTTP/1.1 response order. In practice a takeover is always the first
					// (and only) request on its connection, so the buffer is empty here;
					// if it is not, the peer violated that contract — drop the connection
					// rather than emit responses out of order. (A .manual handler must
					// not append to `out`; guard on the pre-call length.)
					if pre_write_len > cs.write_off {
						close_conn(mut w, fd)
						return false
					}
					if ctl.takeover_mode == .manual {
						// The handler owns the fd from here (SSE/WebSocket).
						detach_conn(mut w, fd)
						return false
					}
					// .reusable: the reactor keeps serving the kept-alive connection.
					set_blocking(fd, false)
					if ctl.should_close || step != .done {
						cs.should_close = true
						compact_read_buf(mut cs, pos)
						mark_active(mut w, mut cs)
						return flush_batch(mut w, fd, mut cs)
					}
					continue
				}
				.none {}
			}

			// step != .done (.close, or .suspend with no watch reactor yet) closes.
			if ctl.should_close || step != .done {
				cs.should_close = true
			}
			if ctl.file_path != '' {
				open_deferred_file(mut cs, ctl.file_path)
			}
		} else {
			// Legacy path: the handler returns a full HttpResponse and the reactor
			// copies it into the shared write buffer. This path keeps the -prealloc
			// request arena.
			mut arena := unsafe { voidptr(nil) }
			$if prealloc {
				arena = unsafe { prealloc_scope_begin() }
			}
			mut decoded := decode_http_request(req_view) or {
				eprintln('Error decoding request ${err}')
				$if prealloc {
					end_request_arena_current_thread(arena)
				}
				cs.write_buf << tiny_bad_request_response
				cs.should_close = true
				compact_read_buf(mut cs, pos)
				mark_active(mut w, mut cs)
				return flush_batch(mut w, fd, mut cs)
			}
			decoded.client_conn_fd = fd
			decoded.client_conn_handle = usize(fd)
			decoded.user_data = w.server.user_data
			decoded.worker_state = w.worker_state
			mut resp := w.server.request_handler(decoded) or {
				eprintln('Error handling request ${err}')
				$if prealloc {
					end_request_arena_current_thread(arena)
				}
				cs.write_buf << tiny_bad_request_response
				cs.should_close = true
				compact_read_buf(mut cs, pos)
				mark_active(mut w, mut cs)
				return flush_batch(mut w, fd, mut cs)
			}

			if resp.takeover_mode != .none && cs.write_buf.len > cs.write_off {
				// A takeover handler already wrote its response straight to the socket.
				// Earlier pipelined responses are still buffered here; flushing them now
				// would put them AFTER the takeover write on the wire — reversing
				// HTTP/1.1 response order. A takeover is always the first (and only)
				// request on its connection in practice, so this buffer is empty; if it
				// is not, the peer violated that contract — drop the connection rather
				// than emit responses out of order.
				resp.free_owned_content()
				$if prealloc {
					end_request_arena_current_thread(arena)
				}
				close_conn(mut w, fd)
				return false
			}
			match resp.takeover_mode {
				.manual {
					// The handler took ownership of the connection: hand off the fd
					// without closing it. Its arena is abandoned to the handler.
					resp.attach_request_arena_if_empty(arena)
					resp.free_owned_content()
					resp.abandon_request_arena_current_thread()
					detach_conn(mut w, fd)
					return false
				}
				.reusable {
					// The handler wrote the response directly to the socket; keep the
					// connection alive (unless it asked to close). A takeover handler may
					// have wrapped the fd in a blocking net.TcpConn for its synchronous
					// write, so restore non-blocking mode before the edge-triggered loop
					// reads from it again.
					set_blocking(fd, false)
					resp.free_owned_content()
					$if prealloc {
						end_request_arena_current_thread(arena)
					}
					if resp.should_close {
						cs.should_close = true
						compact_read_buf(mut cs, pos)
						mark_active(mut w, mut cs)
						return flush_batch(mut w, fd, mut cs)
					}
					continue
				}
				.none {}
			}

			// Normal response: copy its bytes into the shared write buffer. Under
			// -prealloc, leave the request arena first so growing write_buf allocates
			// on the normal heap, not in the (about-to-be-freed) scope.
			$if prealloc {
				leave_request_arena_current_thread(arena)
			}
			if resp.content.len > 0 {
				unsafe { cs.write_buf.push_many(resp.content.data, resp.content.len) }
			}
			if resp.should_close {
				cs.should_close = true
			}
			if resp.file_path != '' {
				open_deferred_file(mut cs, resp.file_path)
			}
			resp.free_owned_content()
			$if prealloc {
				if arena != unsafe { nil } {
					unsafe { prealloc_scope_free_after(arena) }
				}
			}
		}

		if cs.should_close || cs.file_fd != -1 {
			// Batch boundary: a close or a deferred file body ends the batch (gate on
			// file_fd so a zero-length file is drained and closed here, not orphaned).
			// Flush now, then keep draining any requests pipelined behind it —
			// otherwise a request buffered after a file response would be stranded (no
			// new edge fires for bytes already in read_buf) until it spuriously times out.
			compact_read_buf(mut cs, pos)
			mark_active(mut w, mut cs)
			if !flush_batch(mut w, fd, mut cs) {
				return false // connection closed (should_close, or a write error)
			}
			// If the flush parked on EPOLLOUT (still pending), stop; handle_writable
			// resumes and re-drains. Otherwise it fully drained (keep-alive) — carry on.
			if cs.write_off < cs.write_buf.len || cs.file_remaining > 0 {
				return true
			}
			pos = 0
			continue
		}
		// Guard against a peer that pipelines without reading responses.
		if cs.write_buf.len - cs.write_off > max_pending_write {
			cs.should_close = true
			compact_read_buf(mut cs, pos)
			mark_active(mut w, mut cs)
			return flush_batch(mut w, fd, mut cs)
		}
	}
	compact_read_buf(mut cs, pos)
	return true
}

// serve_conn drains the socket into read_buf (edge-triggered, until EAGAIN),
// answers every complete request as it arrives, and flushes the batch once.
@[direct_array_access]
fn serve_conn(mut w Worker, fd int, mut cs ConnState) {
	for {
		if cs.read_buf.len == cs.read_buf.cap {
			target := frame_expected_total(cs.read_buf)
			if target > cs.read_buf.cap {
				unsafe { cs.read_buf.grow_cap(target - cs.read_buf.cap) }
			} else {
				unsafe { cs.read_buf.grow_cap(cs.read_buf.cap) }
			}
		}
		spare := cs.read_buf.cap - cs.read_buf.len
		n := C.recv(fd, unsafe { &u8(cs.read_buf.data) + cs.read_buf.len }, usize(spare), 0)
		if n < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				break // socket drained
			}
			if C.errno == C.EINTR {
				continue
			}
			eprintln('ERROR: recv() failed with errno=${C.errno}')
			C.send(fd, status_444_response.data, status_444_response.len, C.MSG_NOSIGNAL)
			close_conn(mut w, fd)
			return
		}
		if n == 0 {
			// Peer closed (FIN). If there is nothing buffered, this is a clean close.
			close_conn(mut w, fd)
			return
		}
		unsafe {
			cs.read_buf.len += n
		}
		if !drain_requests(mut w, fd, mut cs) {
			return
		}
		// Reject a request whose head cannot fit the configured buffer.
		if cs.read_buf.len > 0 {
			hl := frame_head_len(cs.read_buf)
			too_large := (hl < 0 && cs.read_buf.len >= w.server.max_request_buffer_size)
				|| hl > w.server.max_request_buffer_size
			if too_large {
				// Append the 413 after any responses already buffered for earlier
				// pipelined requests, then flush in order and close (should_close).
				cs.write_buf << status_413_response
				cs.should_close = true
				mark_active(mut w, mut cs)
				flush_batch(mut w, fd, mut cs)
				return
			}
		}
	}
	mark_read_deadline(mut w, mut cs)
	if cs.write_buf.len > cs.write_off || cs.file_remaining > 0 {
		mark_active(mut w, mut cs)
		flush_batch(mut w, fd, mut cs)
	}
}

// handle_writable resumes a parked batch when the socket becomes writable.
@[direct_array_access]
fn handle_writable(mut w Worker, fd int) {
	if fd >= w.conns.len {
		return
	}
	mut cs := w.conns[fd]
	if unsafe { cs == nil } {
		return
	}
	if cs.write_off >= cs.write_buf.len && cs.file_remaining <= 0 {
		// Spurious wake — nothing parked; stop watching writability.
		mod_fd_in_epoll(w.epoll_fd, fd, u32(C.EPOLLIN | C.EPOLLET))
		return
	}
	if !flush_batch(mut w, fd, mut cs) {
		return
	}
	// If the parked batch fully drained (keep-alive) and requests were pipelined
	// behind it, drain them now: their bytes are already in read_buf, so no new
	// EPOLLIN edge will fire for them.
	if cs.write_off >= cs.write_buf.len && cs.file_remaining <= 0 && cs.read_buf.len > 0 {
		if !drain_requests(mut w, fd, mut cs) {
			return
		}
		if cs.write_buf.len > cs.write_off || cs.file_remaining > 0 {
			flush_batch(mut w, fd, mut cs)
		}
	}
}

// handle_accept_loop accepts every pending connection (edge-triggered) and
// registers each with this worker's epoll for readability.
fn handle_accept_loop(mut w Worker) {
	for {
		client_fd := C.accept4(w.listen_fd, C.NULL, C.NULL, C.SOCK_NONBLOCK)
		if client_fd < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				break // no more incoming connections
			}
			if C.errno == C.EINTR {
				continue
			}
			eprintln(@LOCATION)
			C.perror(c'Accept failed')
			break
		}
		// Enable TCP_NODELAY for lower latency.
		opt := 1
		C.setsockopt(client_fd, C.IPPROTO_TCP, C.TCP_NODELAY, &opt, sizeof(opt))
		if add_fd_to_epoll(w.epoll_fd, client_fd, u32(C.EPOLLIN | C.EPOLLET)) == -1 {
			close_socket(client_fd)
			continue
		}
		// Pre-create the pooled state so its buffers are ready on the first read.
		state_for(mut w, client_fd)
	}
}

// sweep_timeouts closes connections whose read/write deadline elapsed. Only runs
// when something is parked, so it costs nothing on an idle/fast server.
@[direct_array_access]
fn sweep_timeouts(mut w Worker) {
	if w.server.timeout_in_seconds <= 0 || w.parked == 0 {
		return
	}
	now := time.sys_mono_now()
	timeout_ns := i64(w.server.timeout_in_seconds) * 1_000_000_000
	for fd in 0 .. w.conns.len {
		cs := w.conns[fd]
		if unsafe { cs == nil } {
			continue
		}
		if cs.read_start_ns > 0 && now - cs.read_start_ns >= timeout_ns {
			C.send(fd, status_408_response.data, status_408_response.len, C.MSG_NOSIGNAL)
			close_conn(mut w, fd)
		} else if cs.write_start_ns > 0 && now - cs.write_start_ns >= timeout_ns {
			close_conn(mut w, fd)
		}
	}
}

// close_worker_clients tears down every open connection this worker owns.
@[direct_array_access]
fn close_worker_clients(mut w Worker) {
	for fd in 0 .. w.conns.len {
		if unsafe { w.conns[fd] != nil } {
			close_conn(mut w, fd)
		}
	}
}

fn process_events(server &Server, epoll_fd int, listen_fd int) {
	mut w := Worker{
		epoll_fd:  epoll_fd
		listen_fd: listen_fd
		conns:     []&ConnState{len: conn_table_min, init: unsafe { nil }}
	}
	unsafe {
		w.server = server
	}
	// Build this worker thread's lock-free per-worker state exactly once.
	if server.make_state != unsafe { nil } {
		w.worker_state = server.make_state()
	}
	mut events := [max_connection_size]C.epoll_event{}
	for {
		if server.is_shutting_down() && server.active_request_count() == 0 {
			close_worker_clients(mut w)
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
			fd := unsafe { events[i].data.fd }
			ev := unsafe { events[i].events }
			// Accept new connections when the listening socket is readable.
			if fd == listen_fd {
				if !server.is_shutting_down() {
					handle_accept_loop(mut w)
				}
				continue
			}
			if ev & u32(C.EPOLLHUP | C.EPOLLERR) != 0 {
				if fd > 0 {
					has_pending := fd < w.conns.len && unsafe { w.conns[fd] != nil }
						&& (w.conns[fd].write_off < w.conns[fd].write_buf.len
						|| w.conns[fd].file_remaining > 0)
					if !has_pending {
						C.send(fd, status_444_response.data, status_444_response.len,
							C.MSG_NOSIGNAL)
					}
					close_conn(mut w, fd)
				}
				continue
			}
			if ev & u32(C.EPOLLOUT) != 0 {
				handle_writable(mut w, fd)
				// If the connection was closed by the writable handler, skip EPOLLIN.
				if fd >= w.conns.len || unsafe { w.conns[fd] == nil } {
					continue
				}
			}
			if ev & u32(C.EPOLLIN) != 0 {
				// No live connection for this fd (closed earlier this batch, or a
				// stale edge): skip it rather than fabricate a phantom connection.
				if fd >= w.conns.len || unsafe { w.conns[fd] == nil } {
					continue
				}
				mut cs := w.conns[fd]
				// While a response is still draining for this fd, defer new requests:
				// reading now would clobber the in-flight write buffer.
				if cs.write_off < cs.write_buf.len || cs.file_remaining > 0 {
					continue
				}
				if server.is_shutting_down() {
					close_conn(mut w, fd)
					continue
				}
				serve_conn(mut w, fd, mut cs)
			}
		}
		sweep_timeouts(mut w)
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

// buf_view returns a non-owning []u8 window over buf[start..start+length] without
// going through array.slice() (which does slice-aliasing bookkeeping per call).
// read_buf is manually managed (grown via grow_cap, compacted via memmove, len
// reset — never delete-d with a live slice), so the marking is pure waste here.
// The window shares read_buf's backing and is read-only and short-lived (the
// parser and handler consume it before the next recv can move read_buf).
@[inline]
fn buf_view(buf []u8, start int, length int) []u8 {
	mut v := unsafe { buf }
	unsafe {
		v.data = &u8(buf.data) + start
		v.len = length
		v.cap = length
		v.flags.clear(.managed)
	}
	return v
}
