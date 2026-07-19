// vtest build: windows
module fasthttp

import net
import time

#define v_fasthttp_test_connect(s, name, namelen) connect((s), (name), (namelen))
#define v_fasthttp_test_getsockname(s, name, namelen) getsockname((s), (name), (namelen))
#define v_fasthttp_test_cancel_io(s, overlapped) CancelIoEx((HANDLE)(s), (overlapped))

fn C.v_fasthttp_test_connect(s C.SOCKET, name voidptr, namelen int) int
fn C.v_fasthttp_test_getsockname(s C.SOCKET, name voidptr, namelen &int) int
fn C.v_fasthttp_test_cancel_io(s C.SOCKET, overlapped &C.OVERLAPPED) int

fn test_accept_wrapper_preserves_pointer_sized_socket() {
	client_fd := C.v_fasthttp_accept(iocp_invalid_socket)
	assert sizeof(client_fd) == sizeof(C.SOCKET)
	assert sizeof(client_fd) == sizeof(usize)
}

const iocp_gc_large_body_size = 8 * 1024 * 1024
const iocp_gc_max_response_header_size = 8192
const iocp_gc_write_payload_size = 4096

fn iocp_gc_large_response() []u8 {
	header := 'HTTP/1.1 200 OK\r\nContent-Length: ${iocp_gc_large_body_size}\r\nConnection: close\r\n\r\n'
	mut content := []u8{cap: header.len + iocp_gc_large_body_size}
	content << header.bytes()
	content << []u8{len: iocp_gc_large_body_size, init: u8(`x`)}
	return content
}

fn iocp_gc_test_handler(req HttpRequest) !HttpResponse {
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
	if path == '/large' {
		send_buffer_size := 4096
		if C.setsockopt(C.SOCKET(req.client_conn_handle), C.SOL_SOCKET, C.SO_SNDBUF,
			voidptr(&send_buffer_size), sizeof(send_buffer_size)) != 0 {
			return error('failed to constrain the large-response send buffer')
		}
		return HttpResponse{
			content:       iocp_gc_large_response()
			content_owned: true
			should_close:  true
		}
	}
	if path == '/collect' {
		for _ in 0 .. 3 {
			gc_collect()
		}
	}
	body := 'OK'
	return HttpResponse{
		content:       'HTTP/1.1 200 OK\r\nContent-Length: ${body.len}\r\nConnection: close\r\n\r\n${body}'.bytes()
		content_owned: true
		should_close:  true
	}
}

fn iocp_gc_test_read_ok_response(mut conn net.TcpConn) ! {
	mut response := []u8{cap: 256}
	mut chunk := []u8{len: 256}
	for response.len < iocp_gc_max_response_header_size {
		read := conn.read(mut chunk)!
		if read == 0 {
			return error('connection closed before the response headers completed')
		}
		response << chunk[..read]
		response_text := response.bytestr()
		if response_text.contains('\r\n\r\n') {
			if !response_text.starts_with('HTTP/1.1 200 OK\r\n') {
				return error('unexpected IOCP regression response status')
			}
			return
		}
	}
	return error('IOCP regression response headers exceeded ${iocp_gc_max_response_header_size} bytes')
}

fn iocp_gc_wait_for_registry_len(server &Server, expected int, timeout time.Duration) ! {
	deadline := time.sys_mono_now() + i64(timeout)
	for {
		mut registry := server.registry
		actual := registry.len()
		if actual == expected {
			return
		}
		if time.sys_mono_now() >= deadline {
			return error('timed out waiting for ${expected} IOCP connections; found ${actual}')
		}
		time.sleep(time.millisecond)
	}
}

fn iocp_gc_listener_port(fd C.SOCKET) !int {
	mut addr := net.new_ip(0, [u8(0), 0, 0, 0]!)
	mut addr_len := int(addr.len())
	if C.v_fasthttp_test_getsockname(fd, voidptr(&addr), &addr_len) != 0 {
		return error(wsa_error_message('getsockname'))
	}
	return int(addr.port()!)
}

fn iocp_gc_dial_with_receive_buffer(port int, size int) !&net.TcpConn {
	addr := net.new_ip(u16(port), [u8(127), 0, 0, 1]!)
	mut socket := net.new_tcp_socket(.ip)!
	socket.set_option_int(.receive_buf_size, size)!
	if C.v_fasthttp_test_connect(C.SOCKET(socket.handle), voidptr(&addr), int(addr.len())) != 0 {
		close_socket(C.SOCKET(socket.handle))
		return error(wsa_error_message('connect'))
	}
	return &net.TcpConn{
		sock:          socket
		read_timeout:  10 * time.second
		write_timeout: 5 * time.second
	}
}

fn iocp_gc_test_request_once(addr string, path string) ! {
	mut conn := net.dial_tcp(addr)!
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(5 * time.second)
	conn.set_write_timeout(5 * time.second)
	conn.write_string('GET ${path} HTTP/1.1\r\nHost: ${addr}\r\nConnection: close\r\n\r\n')!
	iocp_gc_test_read_ok_response(mut conn)!
}

@[noinline]
fn iocp_gc_post_test_write(mut conn IocpConn) bool {
	mut op := &IocpOperation{
		kind: .write
		conn: conn
		buf:  []u8{len: iocp_gc_write_payload_size, init: u8(`x`)}
	}
	if !post_write_op(mut op) {
		op.free()
		return false
	}
	return true
}

fn iocp_gc_cleanup_test_conn(mut conn IocpConn, iocp voidptr, completed voidptr) {
	mut pending_op := conn.pending_op
	raw_pending := pending_op.load()
	if raw_pending != unsafe { nil } && completed == unsafe { nil } {
		pending_overlapped := unsafe { &C.OVERLAPPED(raw_pending) }
		C.v_fasthttp_test_cancel_io(conn.fd, pending_overlapped)
	}

	// Closing the socket cancels any I/O that raced with CancelIoEx. Keep the
	// registry root until IOCP confirms that the kernel has released OVERLAPPED.
	if conn.mark_closing() {
		close_conn_socket(conn.fd)
	}

	mut drained := unsafe { &C.OVERLAPPED(completed) }
	if raw_pending != unsafe { nil } && drained == unsafe { nil } {
		mut bytes_transferred := C.DWORD(0)
		mut completion_key := C.ULONG_PTR(0)
		C.GetQueuedCompletionStatus(iocp, &bytes_transferred, &completion_key, &drained,
			C.DWORD(5000))
		if drained == unsafe { nil } {
			eprintln('timed out draining the cancelled IOCP write regression operation')
			exit(1)
		}
	}
	if drained != unsafe { nil } {
		if raw_pending == unsafe { nil } || voidptr(drained) != raw_pending {
			eprintln('dequeued an unexpected IOCP write regression operation during cleanup')
			exit(1)
		}
		mut op := unsafe { &IocpOperation(drained) }
		if !conn.take_op(drained) {
			eprintln('the IOCP write regression operation changed before cleanup')
			exit(1)
		}
		op.free()
	}
	if pending_op.load() != unsafe { nil } {
		eprintln('the IOCP write regression operation remained pending after cleanup')
		exit(1)
	}
	close_conn(mut conn)
}

fn test_iocp_write_operation_remains_reachable_after_submission() {
	mut test_server := new_server(
		family:  .ip
		port:    0
		handler: iocp_gc_test_handler
	) or {
		assert false, 'failed to create the IOCP write regression server: ${err}'
		return
	}
	test_server.listen_fd = create_server_socket(test_server) or {
		assert false, 'failed to create the IOCP write regression listener: ${err}'
		return
	}
	defer {
		close_socket(test_server.listen_fd)
	}
	test_port := iocp_gc_listener_port(test_server.listen_fd) or {
		assert false, 'failed to discover the IOCP write regression listener port: ${err}'
		return
	}
	test_server.iocp = C.CreateIoCompletionPort(C.INVALID_HANDLE_VALUE, unsafe { nil }, 0,
		C.DWORD(1))
	if test_server.iocp == unsafe { nil } {
		assert false, 'failed to create the dedicated IOCP write regression port'
		return
	}
	defer {
		C.CloseHandle(test_server.iocp)
	}

	mut client := net.dial_tcp('127.0.0.1:${test_port}') or {
		assert false, 'failed to connect the IOCP write regression client: ${err}'
		return
	}
	defer {
		client.close() or {}
	}
	server_fd := C.v_fasthttp_accept(test_server.listen_fd)
	if server_fd == iocp_invalid_socket {
		assert false, 'failed to accept the IOCP write regression client'
		return
	}
	mut conn := &IocpConn{
		server: test_server
		fd:     server_fd
	}
	if !associate_socket_with_iocp(test_server.iocp, server_fd) {
		close_socket(server_fd)
		free_conn_storage(mut conn)
		assert false, 'failed to associate the IOCP write regression socket'
		return
	}
	mut registry := test_server.registry
	registry.add(conn)
	mut cleanup_needed := true
	mut completed_overlapped := &C.OVERLAPPED(unsafe { nil })
	defer {
		if cleanup_needed {
			iocp_gc_cleanup_test_conn(mut conn, test_server.iocp, voidptr(completed_overlapped))
		}
	}

	if !iocp_gc_post_test_write(mut conn) {
		assert false, 'failed to submit the IOCP write regression operation'
		return
	}
	// The posting helper has returned and this dedicated IOCP has no worker, so
	// conn.pending_op is the only V root until the completion is dequeued below.
	for _ in 0 .. 3 {
		gc_collect()
	}
	mut churn := []&IocpOperation{cap: 256}
	for _ in 0 .. 256 {
		churn << &IocpOperation{}
	}

	mut bytes_transferred := C.DWORD(0)
	mut completion_key := C.ULONG_PTR(0)
	mut overlapped := &C.OVERLAPPED(unsafe { nil })
	ok := C.GetQueuedCompletionStatus(test_server.iocp, &bytes_transferred, &completion_key,
		&overlapped, C.DWORD(5000))
	completed_overlapped = overlapped
	if overlapped == unsafe { nil } {
		assert false, 'timed out waiting for the IOCP write regression completion'
		return
	}
	mut op := unsafe { &IocpOperation(overlapped) }
	assert ok
	assert completion_key == C.ULONG_PTR(server_fd)
	assert bytes_transferred > 0
	assert op.conn == conn
	assert op.kind == .write
	assert op.buf.len == iocp_gc_write_payload_size
	assert op.buf.all(it == u8(`x`))
	assert churn.len == 256
	iocp_gc_cleanup_test_conn(mut conn, test_server.iocp, voidptr(completed_overlapped))
	cleanup_needed = false
}

fn test_pending_iocp_operations_remain_reachable_during_gc() {
	mut server := new_server(
		family:  .ip
		port:    0
		handler: iocp_gc_test_handler
	) or {
		assert false, 'failed to create IOCP regression server: ${err}'
		return
	}
	handle := server.handle()
	server_thread := spawn server.run()
	handle.wait_till_running(max_retries: 1000, retry_period_ms: 10) or {
		assert false, 'IOCP regression server did not start: ${err}'
		return
	}
	defer {
		mut shutdown_succeeded := true
		handle.shutdown(timeout: 5 * time.second) or {
			shutdown_succeeded = false
			assert false, 'IOCP regression server shutdown failed: ${err}'
		}
		if shutdown_succeeded {
			server_thread.wait() or { assert false, 'IOCP regression server thread failed: ${err}' }
		}
	}
	test_port := iocp_gc_listener_port(server.listen_fd) or {
		assert false, 'failed to discover the IOCP regression listener port: ${err}'
		return
	}
	test_addr := '127.0.0.1:${test_port}'
	test_request := 'GET / HTTP/1.1\r\nHost: ${test_addr}\r\nConnection: close\r\n\r\n'

	mut pending := []&net.TcpConn{}
	defer {
		for mut conn in pending {
			conn.close() or {}
		}
	}
	for _ in 0 .. 64 {
		mut conn := net.dial_tcp(test_addr) or {
			assert false, 'failed to open pending IOCP socket: ${err}'
			return
		}
		conn.set_read_timeout(5 * time.second)
		conn.set_write_timeout(5 * time.second)
		pending << conn
	}

	iocp_gc_test_request_once(test_addr, '/collect') or {
		assert false, 'forced-GC request failed: ${err}'
		return
	}
	for _ in 0 .. 256 {
		iocp_gc_test_request_once(test_addr, '/') or {
			assert false, 'IOCP allocation-churn request failed: ${err}'
			return
		}
	}

	for mut conn in pending {
		conn.write_string(test_request) or {
			assert false, 'pending IOCP socket write failed after GC: ${err}'
			return
		}
		iocp_gc_test_read_ok_response(mut conn) or {
			assert false, 'pending IOCP socket read failed after GC: ${err}'
			return
		}
	}
	iocp_gc_wait_for_registry_len(server, 0, 5 * time.second) or {
		assert false, 'pending IOCP connections did not close: ${err}'
		return
	}

	mut large_conn := iocp_gc_dial_with_receive_buffer(test_port, 1024) or {
		assert false, 'failed to open large-response IOCP socket: ${err}'
		return
	}
	defer {
		large_conn.close() or {}
	}
	large_conn.set_read_timeout(10 * time.second)
	large_conn.set_write_timeout(5 * time.second)
	large_conn.write_string('GET /large HTTP/1.1\r\nHost: ${test_addr}\r\nConnection: close\r\n\r\n') or {
		assert false, 'large-response IOCP request failed: ${err}'
		return
	}
	for _ in 0 .. 64 {
		iocp_gc_test_request_once(test_addr, '/') or {
			assert false, 'IOCP write allocation-churn request failed: ${err}'
			return
		}
	}
	large_header := 'HTTP/1.1 200 OK\r\nContent-Length: ${iocp_gc_large_body_size}\r\nConnection: close\r\n\r\n'
	expected_size := large_header.len + iocp_gc_large_body_size
	mut received := []u8{cap: expected_size}
	mut chunk := []u8{len: 64 * 1024}
	for received.len < expected_size {
		read := large_conn.read(mut chunk) or {
			assert false, 'large-response IOCP read failed after GC: ${err}'
			return
		}
		if read == 0 {
			break
		}
		received << chunk[..read]
	}
	assert received.len == expected_size
	assert received[..large_header.len].bytestr() == large_header
	assert received[large_header.len..].all(it == u8(`x`))
	iocp_gc_wait_for_registry_len(server, 0, 5 * time.second) or {
		assert false, 'large-response IOCP connection did not close: ${err}'
		return
	}
}
