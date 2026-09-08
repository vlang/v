module fasthttp

import net
import os
import sync
import sync.stdatomic
import time

const iocp_wait_timeout_ms = 100
const iocp_read_buf_size = 8192
const iocp_thread_count = max_thread_pool_size + 1
const iocp_transmit_file_max_chunk_size = i64(2_147_483_646)
const status_408_response = 'HTTP/1.1 408 Request Timeout\r\nContent-Type: text/plain\r\nContent-Length: 19\r\nConnection: close\r\n\r\n408 Request Timeout'.bytes()

enum IocpOperationKind {
	read
	write
	transmit_file
}

@[heap]
struct IocpConn {
	server &Server
	fd     C.SOCKET
mut:
	request_buf []u8
	request_len int
	read_eof    bool
	// Housekeeping runs concurrently with completion workers, so fields it reads
	// are atomic rather than merely protected by the registry container mutex.
	read_start     &stdatomic.AtomicVal[u64] = stdatomic.new_atomic(u64(0))
	write_start    &stdatomic.AtomicVal[u64] = stdatomic.new_atomic(u64(0))
	request_active &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	request_arena  voidptr
	should_close   bool
	closing        &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	// The registry keeps the connection reachable, and this pointer keeps the one
	// outstanding kernel-owned operation reachable until IOCP completes it.
	pending_op  &stdatomic.AtomicVal[voidptr] = stdatomic.new_atomic(voidptr(unsafe { nil }))
	file_handle voidptr
	has_file    bool
	file_len    i64
	file_pos    i64
}

struct IocpConnRegistry {
mut:
	mutex &sync.Mutex = sync.new_mutex()
	conns map[voidptr]&IocpConn
}

@[heap]
struct IocpOperation {
mut:
	overlapped C.OVERLAPPED
	wsabuf     C.WSABUF
	kind       IocpOperationKind
	conn       &IocpConn = unsafe { nil }
	buf        []u8
	pos        int
}

pub struct Server {
pub:
	family                  net.AddrFamily = .ip6
	host                    string
	port                    int = 3000
	max_request_buffer_size int = 8192
	max_request_body_size   int = default_max_request_body_size
	timeout_in_seconds      int = 30
	user_data               voidptr
mut:
	listen_fd       C.SOCKET = iocp_invalid_socket
	iocp            voidptr
	threads         []thread = []thread{len: iocp_thread_count, cap: iocp_thread_count}
	registry        &IocpConnRegistry = &IocpConnRegistry{}
	request_handler fn (HttpRequest) !HttpResponse = unsafe { nil }
	append_handler  AppendHandler = unsafe { nil }
	make_state      fn () voidptr = unsafe { nil }
	running         &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	shutting_down   &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	stopped         &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(true)
	active_requests &stdatomic.AtomicVal[int] = stdatomic.new_atomic(0)
}

// new_server creates and initializes a new Server instance.
pub fn new_server(config ServerConfig) !&Server {
	if config.max_request_buffer_size <= 0 {
		return error('max_request_buffer_size must be greater than 0')
	}
	if config.max_request_body_size < 0 {
		return error('max_request_body_size must not be negative')
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
		family: config.family
		host: config.host
		port: config.port
		max_request_buffer_size: config.max_request_buffer_size
		max_request_body_size: config.max_request_body_size
		timeout_in_seconds: config.timeout_in_seconds
		user_data: config.user_data
		request_handler: config.handler
		append_handler: config.append_handler
		make_state: config.make_state
		running: stdatomic.new_atomic(false)
		shutting_down: stdatomic.new_atomic(false)
		stopped: stdatomic.new_atomic(true)
		active_requests: stdatomic.new_atomic(0)
		registry: &IocpConnRegistry{
			mutex: sync.new_mutex()
		}
	}
	unsafe {
		server.threads.flags.set(.noslices | .noshrink | .nogrow)
	}
	return server
}

fn socket_handle(fd C.SOCKET) voidptr {
	return voidptr(fd)
}

fn close_socket(fd C.SOCKET) {
	if fd != iocp_invalid_socket {
		C.closesocket(fd)
	}
}

fn get_transmit_file_fn(fd C.SOCKET) ?IocpTransmitFileFn {
	mut transmit_file_guid := C.GUID{
		Data1: 0xb5367df0
		Data2: 0xcbac
		Data3: 0x11cf
		Data4: [u8(0x95), 0xca, 0x00, 0x80, 0x5f, 0x48, 0xa1, 0x92]!
	}
	mut transmit_file := voidptr(unsafe { nil })
	mut bytes_returned := C.DWORD(0)
	rc := C.WSAIoctl(fd, C.SIO_GET_EXTENSION_FUNCTION_POINTER, voidptr(&transmit_file_guid), C.DWORD(sizeof(C.GUID)), voidptr(&transmit_file), C.DWORD(sizeof(voidptr)), &bytes_returned, unsafe { nil }, unsafe { nil })
	if rc == C.SOCKET_ERROR || transmit_file == unsafe { nil } {
		return none
	}
	return IocpTransmitFileFn(transmit_file)
}

fn wsa_error_message(label string) string {
	return '${label} failed with WSA error ${C.WSAGetLastError()}'
}

fn create_server_socket(server &Server) !C.SOCKET {
	// Resolve the bind address first: the socket family must match the address
	// family, and a configured host may resolve to a different family than
	// server.family (e.g. an IPv4 host with the default family: .ip6).
	addr := resolve_bind_addr(server.host, server.family, server.port)!
	server_fd := C.WSASocketW(i32(addr.family()), i32(net.SocketType.tcp), 0, unsafe { nil }, 0, u32(C.WSA_FLAG_OVERLAPPED))
	if server_fd == iocp_invalid_socket {
		return error(wsa_error_message('WSASocketW'))
	}

	opt := 1
	if C.v_fasthttp_setsockopt(server_fd, C.SOL_SOCKET, C.SO_REUSEADDR, voidptr(&opt), sizeof(opt)) < 0 {
		close_socket(server_fd)
		return error(wsa_error_message('setsockopt SO_REUSEADDR'))
	}
	if addr.family() == .ip6 {
		ipv6_only := 0
		C.v_fasthttp_setsockopt(server_fd, C.IPPROTO_IPV6, C.IPV6_V6ONLY, voidptr(&ipv6_only), sizeof(ipv6_only))
	}

	if C.v_fasthttp_bind(server_fd, voidptr(&addr), int(addr.len())) < 0 {
		close_socket(server_fd)
		return error(wsa_error_message('bind'))
	}
	if C.v_fasthttp_listen(server_fd, max_connection_size) < 0 {
		close_socket(server_fd)
		return error(wsa_error_message('listen'))
	}
	return server_fd
}

fn associate_socket_with_iocp(iocp voidptr, fd C.SOCKET) bool {
	return C.CreateIoCompletionPort(socket_handle(fd), iocp, C.ULONG_PTR(fd), 0) != unsafe { nil }
}

fn (mut conn IocpConn) mark_closing() bool {
	mut closing := conn.closing
	return closing.compare_and_swap(false, true)
}

fn (conn &IocpConn) is_closing() bool {
	mut closing := conn.closing
	return closing.load()
}

fn (mut conn IocpConn) register_op(op &IocpOperation) {
	mut pending_op := conn.pending_op
	if !pending_op.compare_and_swap(voidptr(unsafe { nil }), voidptr(op)) {
		panic('fasthttp: attempted to post concurrent operations on one IOCP connection')
	}
}

fn (mut conn IocpConn) unregister_op(op &IocpOperation) {
	mut pending_op := conn.pending_op
	if !pending_op.compare_and_swap(voidptr(op), voidptr(unsafe { nil })) {
		panic('fasthttp: pending IOCP operation changed before submission failed')
	}
}

fn (mut conn IocpConn) take_op(overlapped &C.OVERLAPPED) bool {
	mut pending_op := conn.pending_op
	raw_op := voidptr(overlapped)
	return pending_op.compare_and_swap(raw_op, voidptr(unsafe { nil }))
}

fn close_conn_socket(fd C.SOCKET) {
	C.v_fasthttp_shutdown(fd, C.SD_BOTH)
	close_socket(fd)
}

fn conn_registry_key(conn &IocpConn) voidptr {
	return voidptr(conn)
}

fn (mut registry IocpConnRegistry) add(conn &IocpConn) {
	registry.mutex.lock()
	registry.conns[conn_registry_key(conn)] = conn
	registry.mutex.unlock()
}

fn (mut registry IocpConnRegistry) remove(conn &IocpConn) {
	registry.mutex.lock()
	registry.conns.delete(conn_registry_key(conn))
	registry.mutex.unlock()
}

fn (mut registry IocpConnRegistry) len() int {
	registry.mutex.lock()
	n := registry.conns.len
	registry.mutex.unlock()
	return n
}

fn (mut registry IocpConnRegistry) finish_conn(mut conn IocpConn) {
	registry.mutex.lock()
	if conn.mark_closing() {
		close_conn_socket(conn.fd)
	}
	registry.conns.delete(conn_registry_key(conn))
	registry.mutex.unlock()
}

fn (mut registry IocpConnRegistry) close_all_async() {
	registry.mutex.lock()
	for key in registry.conns.keys() {
		mut conn := registry.conns[key] or { continue }
		if conn.mark_closing() {
			close_conn_socket(conn.fd)
		}
	}
	registry.mutex.unlock()
}

fn (mut registry IocpConnRegistry) sweep_timed_out_io(timeout_ns u64) {
	now := time.sys_mono_now()
	registry.mutex.lock()
	for key in registry.conns.keys() {
		mut conn := registry.conns[key] or { continue }
		if conn.is_closing() {
			continue
		}
		read_start := conn.read_start.load()
		write_start := conn.write_start.load()
		if !conn.request_active.load() && read_start > 0 && now - read_start >= timeout_ns
			&& conn.mark_closing() {
			C.v_fasthttp_send(conn.fd, status_408_response.data, status_408_response.len, 0)
			close_conn_socket(conn.fd)
		}
		if write_start > 0 && now - write_start >= timeout_ns && conn.mark_closing() {
			close_conn_socket(conn.fd)
		}
	}
	registry.mutex.unlock()
}

fn (mut conn IocpConn) close_response_file() {
	if conn.has_file {
		C.CloseHandle(conn.file_handle)
		conn.file_handle = unsafe { nil }
		conn.has_file = false
		conn.file_len = 0
		conn.file_pos = 0
	}
}

fn (mut conn IocpConn) open_response_file(path string) bool {
	conn.close_response_file()
	st := os.stat(path) or { return false }
	path_wide := path.to_wide()
	handle := C.CreateFileW(path_wide, C.GENERIC_READ, C.FILE_SHARE_READ, 0, C.OPEN_EXISTING, C.FILE_ATTRIBUTE_NORMAL, 0)
	if handle == C.INVALID_HANDLE_VALUE || handle == unsafe { nil } {
		return false
	}
	conn.file_handle = handle
	conn.has_file = true
	conn.file_len = i64(st.size)
	conn.file_pos = 0
	return true
}

fn (mut op IocpOperation) free() {
	if op.buf.cap > 0 {
		unsafe { op.buf.free() }
		op.buf = []u8{}
	}
	unsafe { free(op) }
}

fn (mut conn IocpConn) free_request_arena() {
	$if prealloc {
		if conn.request_arena != unsafe { nil } {
			unsafe { prealloc_scope_free_after(conn.request_arena) }
		}
	}
	conn.request_arena = unsafe { nil }
}

fn (mut conn IocpConn) end_active_request() {
	mut request_active := conn.request_active
	if request_active.compare_and_swap(true, false) {
		conn.server.end_request()
	}
}

fn (mut conn IocpConn) begin_active_request() {
	mut request_active := conn.request_active
	if request_active.compare_and_swap(false, true) {
		conn.server.begin_request()
	}
}

fn free_conn_storage(mut conn IocpConn) {
	if conn.request_buf.cap > 0 {
		unsafe { conn.request_buf.free() }
		conn.request_buf = []u8{}
	}
	if conn.closing != unsafe { nil } {
		unsafe { free(conn.closing) }
		conn.closing = unsafe { nil }
	}
	if conn.pending_op != unsafe { nil } {
		mut pending_op := conn.pending_op
		if pending_op.load() != unsafe { nil } {
			panic('fasthttp: freeing an IOCP connection with a pending operation')
		}
		unsafe { free(conn.pending_op) }
		conn.pending_op = unsafe { nil }
	}
	if conn.read_start != unsafe { nil } {
		unsafe { free(conn.read_start) }
		conn.read_start = unsafe { nil }
	}
	if conn.write_start != unsafe { nil } {
		unsafe { free(conn.write_start) }
		conn.write_start = unsafe { nil }
	}
	if conn.request_active != unsafe { nil } {
		unsafe { free(conn.request_active) }
		conn.request_active = unsafe { nil }
	}
	unsafe { free(conn) }
}

fn close_conn(mut conn IocpConn) {
	mut registry := conn.server.registry
	registry.finish_conn(mut conn)
	conn.end_active_request()
	conn.close_response_file()
	conn.free_request_arena()
	free_conn_storage(mut conn)
}

fn release_conn_without_closing(mut conn IocpConn) {
	mut registry := conn.server.registry
	registry.remove(conn)
	conn.end_active_request()
	conn.close_response_file()
	conn.free_request_arena()
	free_conn_storage(mut conn)
}

fn reset_conn_for_next_request(mut conn IocpConn) {
	conn.end_active_request()
	conn.close_response_file()
	conn.free_request_arena()
	if conn.request_len > 0 {
		conn.request_buf.delete_many(0, conn.request_len)
		conn.request_len = 0
	}
	conn.read_start.store(if conn.request_buf.len > 0 { time.sys_mono_now() } else { u64(0) })
	conn.write_start.store(0)
	conn.should_close = false
}

fn post_recv(mut conn IocpConn) bool {
	mut op := &IocpOperation{
		kind: .read
		conn: conn
		buf: []u8{len: iocp_read_buf_size}
	}
	op.wsabuf = C.WSABUF{
		len: u32(op.buf.len)
		buf: &char(op.buf.data)
	}
	mut flags := C.DWORD(0)
	mut bytes_read := C.DWORD(0)
	conn.register_op(op)
	ret := C.WSARecv(conn.fd, &op.wsabuf, 1, &bytes_read, &flags, &op.overlapped, unsafe { nil })
	if ret == C.SOCKET_ERROR {
		code := C.WSAGetLastError()
		if code != C.WSA_IO_PENDING {
			conn.unregister_op(op)
			op.free()
			return false
		}
	}
	return true
}

fn post_write_op(mut op IocpOperation) bool {
	if op.pos >= op.buf.len {
		return true
	}
	op.overlapped = C.OVERLAPPED{}
	remaining := op.buf.len - op.pos
	op.wsabuf = C.WSABUF{
		len: u32(remaining)
		buf: unsafe { &char(&op.buf[op.pos]) }
	}
	mut bytes_sent := C.DWORD(0)
	op.conn.write_start.store(time.sys_mono_now())
	op.conn.register_op(op)
	ret := C.WSASend(op.conn.fd, &op.wsabuf, 1, &bytes_sent, 0, &op.overlapped, unsafe { nil })
	if ret == C.SOCKET_ERROR {
		code := C.WSAGetLastError()
		if code != C.WSA_IO_PENDING {
			op.conn.unregister_op(op)
			return false
		}
	}
	return true
}

fn post_transmit_file(mut conn IocpConn) bool {
	if !conn.has_file {
		return true
	}
	remaining := conn.file_len - conn.file_pos
	if remaining <= 0 {
		return true
	}
	mut chunk_size := remaining
	if chunk_size > iocp_transmit_file_max_chunk_size {
		chunk_size = iocp_transmit_file_max_chunk_size
	}
	mut op := &IocpOperation{
		kind: .transmit_file
		conn: conn
	}
	file_pos := u64(conn.file_pos)
	op.overlapped.Offset = u32(file_pos & 0xffffffff)
	op.overlapped.OffsetHigh = u32(file_pos >> 32)
	conn.write_start.store(time.sys_mono_now())
	transmit_file := get_transmit_file_fn(conn.fd) or {
		op.free()
		return false
	}
	conn.register_op(op)
	ret := transmit_file(conn.fd, conn.file_handle, C.DWORD(chunk_size), 0, &op.overlapped, unsafe { nil }, 0)
	if ret == 0 {
		code := C.WSAGetLastError()
		if code != C.WSA_IO_PENDING {
			conn.unregister_op(op)
			op.free()
			return false
		}
	}
	return true
}

fn continue_response_after_write(mut conn IocpConn, worker_state voidptr) {
	if conn.has_file {
		if conn.file_pos >= conn.file_len {
			conn.close_response_file()
			complete_response(mut conn, worker_state)
			return
		}
		if !post_transmit_file(mut conn) {
			close_conn(mut conn)
		}
		return
	}
	complete_response(mut conn, worker_state)
}

fn complete_response(mut conn IocpConn, worker_state voidptr) {
	should_close := conn.should_close
	reset_conn_for_next_request(mut conn)
	if conn.server.is_shutting_down() || should_close {
		close_conn(mut conn)
		return
	}
	if conn.request_buf.len > 0 {
		dispatch_iocp_request(mut conn, worker_state)
		return
	}
	if conn.read_eof {
		close_conn(mut conn)
		return
	}
	if !post_recv(mut conn) {
		close_conn(mut conn)
	}
}

fn send_response(mut conn IocpConn, content []u8, should_close bool, worker_state voidptr) {
	conn.should_close = should_close
	if content.len == 0 {
		continue_response_after_write(mut conn, worker_state)
		return
	}
	mut op := &IocpOperation{
		kind: .write
		conn: conn
		buf: content
	}
	if !post_write_op(mut op) {
		op.free()
		close_conn(mut conn)
	}
}

fn send_terminal_response(mut conn IocpConn, response []u8, worker_state voidptr) {
	conn.begin_active_request()
	conn.request_len = conn.request_buf.len
	conn.read_start.store(0)
	send_response(mut conn, response.clone(), true, worker_state)
}

fn process_request(mut conn IocpConn, frame_total int, worker_state voidptr) {
	conn.begin_active_request()
	conn.request_len = frame_total
	// The append handler manages its own -prealloc scope; only the legacy path
	// uses the reactor arena.
	using_append := conn.server.append_handler != unsafe { nil }
	mut request_arena := voidptr(unsafe { nil })
	$if prealloc {
		if !using_append {
			request_arena = unsafe { prealloc_scope_begin() }
		}
	}

	// The consumed prefix remains stable until this response completes.
	req_view := conn.request_buf[..frame_total]
	mut decoded := decode_http_request(req_view) or {
		end_request_arena_current_thread(request_arena)
		send_terminal_response(mut conn, tiny_bad_request_response, worker_state)
		return
	}
	// Keep legacy int fd consumers working; client_conn_handle preserves the full SOCKET.
	decoded.client_conn_fd = int(conn.fd)
	decoded.client_conn_handle = usize(conn.fd)
	decoded.user_data = conn.server.user_data
	decoded.worker_state = worker_state

	mut response := if using_append {
		// Zero-copy contract: the handler appends its response into `out`; wrap it
		// into the existing response-sending path.
		// NOTE: `out` is a fresh per-request buffer. Under the default GC it is
		// reclaimed after the send; under -prealloc the append path opens no request
		// scope, so it is not reclaimed per request (known limitation — IOCP does not
		// yet reuse a persistent per-connection write buffer).
		mut out := []u8{}
		mut ctl := ResponseControl{}
		step := conn.server.append_handler(decoded, mut out, worker_state, mut ctl)
		HttpResponse{
			content: out
			content_owned: true
			takeover_mode: ctl.takeover_mode
			should_close: ctl.should_close || step != .done
			file_path: ctl.file_path
		}
	} else {
		conn.server.request_handler(decoded) or {
			end_request_arena_current_thread(request_arena)
			send_terminal_response(mut conn, tiny_bad_request_response, worker_state)
			return
		}
	}
	response.attach_request_arena_if_empty(request_arena)

	match response.takeover_mode {
		.manual {
			response.free_owned_content()
			response.abandon_request_arena_current_thread()
			release_conn_without_closing(mut conn)
			return
		}
		.reusable {
			should_close := response.should_close
				|| (conn.read_eof && frame_total == conn.request_buf.len)
			response.free_owned_content()
			response.end_request_arena_current_thread()
			reset_conn_for_next_request(mut conn)
			if conn.server.is_shutting_down() || should_close {
				close_conn(mut conn)
				return
			}
			if conn.request_buf.len > 0 {
				dispatch_iocp_request(mut conn, worker_state)
			} else if conn.read_eof {
				close_conn(mut conn)
			} else if !post_recv(mut conn) {
				close_conn(mut conn)
			}
			return
		}
		.none {}
	}

	if response.file_path != '' {
		if !conn.open_response_file(response.file_path) {
			response.free_owned_content()
			response.end_request_arena_current_thread()
			send_terminal_response(mut conn, tiny_bad_request_response, worker_state)
			return
		}
	}
	mut content := response.take_or_clone_content()
	conn.request_arena = response.take_request_arena()
	leave_request_arena_current_thread(conn.request_arena)
	should_close := response.should_close || (conn.read_eof && frame_total == conn.request_buf.len)
	send_response(mut conn, content, should_close, worker_state)
}

fn dispatch_iocp_request(mut conn IocpConn, worker_state voidptr) {
	if conn.request_buf.len == 0 {
		if conn.read_eof {
			close_conn(mut conn)
		} else if !post_recv(mut conn) {
			close_conn(mut conn)
		}
		return
	}
	frame_total := frame_request_length_lim_idx(conn.request_buf, conn.server.max_request_buffer_size, conn.server.max_request_body_size)
	if frame_total == -1 {
		if conn.read_eof {
			send_terminal_response(mut conn, tiny_bad_request_response, worker_state)
			return
		}
		read_start := conn.read_start.load()
		if conn.server.timeout_in_seconds > 0 && read_start > 0 {
			timeout_ns := u64(conn.server.timeout_in_seconds) * 1_000_000_000
			if time.sys_mono_now() - read_start >= timeout_ns {
				send_terminal_response(mut conn, status_408_response, worker_state)
				return
			}
		}
		if !post_recv(mut conn) {
			close_conn(mut conn)
		}
		return
	}
	if frame_total < -1 {
		send_terminal_response(mut conn, response_for_frame_error(frame_total), worker_state)
		return
	}
	conn.read_start.store(0)
	process_request(mut conn, frame_total, worker_state)
}

fn handle_read_completion(mut conn IocpConn, mut op IocpOperation, bytes_read C.DWORD, worker_state voidptr) {
	defer {
		op.free()
	}
	if conn.is_closing() {
		close_conn(mut conn)
		return
	}
	if bytes_read == 0 {
		conn.read_eof = true
		dispatch_iocp_request(mut conn, worker_state)
		return
	}

	if conn.read_start.load() == 0 {
		conn.read_start.store(time.sys_mono_now())
	}
	conn.request_buf << op.buf[..int(bytes_read)]
	dispatch_iocp_request(mut conn, worker_state)
}

fn handle_write_completion(mut conn IocpConn, mut op IocpOperation, bytes_written C.DWORD, worker_state voidptr) {
	if conn.is_closing() {
		op.free()
		close_conn(mut conn)
		return
	}
	op.pos += int(bytes_written)
	if op.pos < op.buf.len {
		if !post_write_op(mut op) {
			op.free()
			close_conn(mut conn)
		}
		return
	}
	op.free()
	continue_response_after_write(mut conn, worker_state)
}

fn handle_transmit_file_completion(mut conn IocpConn, mut op IocpOperation, bytes_transferred C.DWORD, worker_state voidptr) {
	if conn.is_closing() {
		op.free()
		close_conn(mut conn)
		return
	}
	if bytes_transferred == 0 && conn.file_pos < conn.file_len {
		op.free()
		close_conn(mut conn)
		return
	}
	conn.file_pos += i64(bytes_transferred)
	op.free()
	continue_response_after_write(mut conn, worker_state)
}

fn run_iocp_housekeeping(server &Server) bool {
	mut registry := server.registry
	if server.timeout_in_seconds > 0 {
		timeout_ns := u64(server.timeout_in_seconds) * 1_000_000_000
		registry.sweep_timed_out_io(timeout_ns)
	}
	if server.is_shutting_down() && server.active_request_count() == 0 {
		registry.close_all_async()
		return registry.len() == 0
	}
	return false
}

fn process_iocp_events(server &Server) {
	mut worker_state := voidptr(unsafe { nil })
	if server.make_state != unsafe { nil } {
		worker_state = server.make_state()
	}
	for {
		if run_iocp_housekeeping(server) {
			return
		}
		mut bytes_transferred := C.DWORD(0)
		mut completion_key := C.ULONG_PTR(0)
		mut overlapped := &C.OVERLAPPED(unsafe { nil })
		ok := C.GetQueuedCompletionStatus(server.iocp, &bytes_transferred, &completion_key, &overlapped, iocp_wait_timeout_ms)
		if overlapped == unsafe { nil } {
			if run_iocp_housekeeping(server) {
				return
			}
			continue
		}

		mut op := unsafe { &IocpOperation(overlapped) }
		mut conn := op.conn
		if !conn.take_op(overlapped) {
			continue
		}
		if !ok {
			op.free()
			close_conn(mut conn)
			continue
		}
		match op.kind {
			.read {
				handle_read_completion(mut conn, mut op, bytes_transferred, worker_state)
			}
			.write {
				handle_write_completion(mut conn, mut op, bytes_transferred, worker_state)
			}
			.transmit_file {
				handle_transmit_file_completion(mut conn, mut op, bytes_transferred, worker_state)
			}
		}

		if run_iocp_housekeeping(server) {
			return
		}
	}
}

fn accept_loop(server &Server) {
	for !server.is_shutting_down() {
		client_fd := C.v_fasthttp_accept(server.listen_fd)
		if client_fd == iocp_invalid_socket {
			if server.is_shutting_down() {
				break
			}
			time.sleep(10 * time.millisecond)
			continue
		}
		opt := 1
		C.v_fasthttp_setsockopt(client_fd, C.IPPROTO_TCP, C.TCP_NODELAY, voidptr(&opt), sizeof(opt))
		mut conn := &IocpConn{
			server: server
			fd: client_fd
			request_buf: []u8{cap: server.max_request_buffer_size}
			closing: stdatomic.new_atomic(false)
			pending_op: stdatomic.new_atomic(voidptr(unsafe { nil }))
		}
		if !associate_socket_with_iocp(server.iocp, client_fd) {
			close_socket(client_fd)
			free_conn_storage(mut conn)
			continue
		}
		mut registry := server.registry
		registry.add(conn)
		if !post_recv(mut conn) {
			close_conn(mut conn)
		}
	}
}

fn (mut server Server) stop_accepting() {
	if server.listen_fd != iocp_invalid_socket {
		C.v_fasthttp_shutdown(server.listen_fd, C.SD_BOTH)
		close_socket(server.listen_fd)
		server.listen_fd = iocp_invalid_socket
	}
	if server.iocp != unsafe { nil } {
		for _ in 0 .. max_thread_pool_size {
			C.PostQueuedCompletionStatus(server.iocp, 0, 0, &C.OVERLAPPED(unsafe { nil }))
		}
	}
}

// run starts the server and begins listening for incoming connections.
pub fn (mut server Server) run() ! {
	server.listen_fd = create_server_socket(server)!
	server.iocp = C.CreateIoCompletionPort(C.INVALID_HANDLE_VALUE, unsafe { nil }, 0, C.DWORD(max_thread_pool_size))
	if server.iocp == unsafe { nil } {
		close_socket(server.listen_fd)
		server.listen_fd = iocp_invalid_socket
		return error('CreateIoCompletionPort failed with error ${C.GetLastError()}')
	}

	for i := 0; i < max_thread_pool_size; i++ {
		server.threads[i] = spawn process_iocp_events(server)
	}
	server.threads[max_thread_pool_size] = spawn accept_loop(server)

	server.mark_running()
	println('listening on http://${listen_host_display(server.host, server.family)}:${server.port}/')

	for i in 0 .. max_thread_pool_size + 1 {
		server.threads[i].wait()
	}
	if server.iocp != unsafe { nil } {
		C.CloseHandle(server.iocp)
		server.iocp = unsafe { nil }
	}
	server.mark_stopped()
}
