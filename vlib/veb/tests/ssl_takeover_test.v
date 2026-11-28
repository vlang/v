// vtest build: !sanitized_job? && (!use_openssl? || (present_openssl? && !(windows && tinyc)))
import io
import net.ssl
import net.websocket
import os
import time
import veb

const port = 13094
const websocket_key = 'dGhlIHNhbXBsZSBub25jZQ=='
const close_payload = [u8(0x03), 0xe8]

struct Context {
	veb.Context
}

struct App {
	started  chan bool
	finished chan bool
}

// before_accept_loop signals that the HTTPS listener is ready.
pub fn (app &App) before_accept_loop() {
	app.started <- true
}

// health serves a regular HTTPS response before the client upgrades.
@['/health']
pub fn (app &App) health(mut ctx Context) veb.Result {
	return ctx.text('secure')
}

// upgrade hands the TLS session to a WebSocket worker.
@['/ws']
pub fn (app &App) upgrade(mut ctx Context) veb.Result {
	key := ctx.req.header.get_custom('Sec-WebSocket-Key') or { '' }
	assert (ctx.tls_socket_handle() or { -1 }) >= 0
	mut conn := ctx.takeover_tls_conn() or { return ctx.server_error(err.msg()) }
	assert ctx.tls_socket_handle() == none
	if mut duplicate := ctx.takeover_tls_conn() {
		duplicate.close() or {}
		panic('TLS connection was taken over twice')
	}
	spawn serve_websocket(mut conn, key, 'idle' in ctx.query, app.finished)
	return veb.no_result()
}

struct TrackedTransport {
mut:
	conn        &veb.TlsConn
	close_calls int
}

fn (mut transport TrackedTransport) read(mut buf []u8) !int {
	return transport.conn.read(mut buf)
}

fn (mut transport TrackedTransport) write(buf []u8) !int {
	return transport.conn.write(buf)
}

fn (mut transport TrackedTransport) close() ! {
	transport.close_calls++
	transport.conn.close()!
}

fn serve_websocket(mut conn veb.TlsConn, key string, check_idle bool, finished chan bool) {
	if check_idle {
		// The client waits for the upgrade response before writing its first frame.
		mut idle := []u8{len: 1}
		if _ := conn.read_with_timeout(mut idle, 20 * time.millisecond) {
			panic('read on an idle TLS connection unexpectedly succeeded')
		} else {
			assert err !is io.Eof
		}
	}

	mut transport := &TrackedTransport{ conn: conn }
	mut server := websocket.new_server(.ip, 0, '/ws')
	server.on_message(fn (mut client websocket.Client, message &websocket.Message) ! {
		assert message.opcode == .text_frame
		client.write_string('echo: ' + message.payload.bytestr())!
	})
	if _ := server.handle_transport_handshake(transport, true, key) {
		assert key != ''
	} else {
		assert key == ''
		assert err.msg() == 'unexpected seckey length zero'
	}
	assert transport.close_calls == 1
	assert rlock server.server_state {
		server.server_state.clients.len
	} == 0
	finished <- true
}

fn masked_frame(opcode u8, payload []u8) []u8 {
	mask := [u8(0x12), 0x34, 0x56, 0x78]
	mut frame := [opcode, u8(0x80 | payload.len)]
	frame << mask
	for i, byte in payload {
		frame << (byte ^ mask[i % 4])
	}
	return frame
}

fn read_headers(mut reader io.BufferedReader) ![]string {
	mut headers := []string{}
	for {
		line := reader.read_line()!
		if line == '' {
			return headers
		}
		headers << line
	}
	return headers
}

fn check_websocket(prefetch bool, finished chan bool) ! {
	mut client := ssl.new_ssl_conn()!
	client.dial('127.0.0.1', port)!
	mut closed := false
	defer {
		if !closed {
			client.close() or {}
		}
	}
	client.set_read_timeout(3 * time.second)
	mut reader := io.new_buffered_reader(reader: client)
	defer { reader.free() }

	// Verify HTTPS keep-alive on the same connection before upgrading.
	client.write_string('GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n')!
	assert read_headers(mut reader)![0] == 'HTTP/1.1 200 OK'
	mut body := []u8{len: 'secure'.len}
	io.read_full(mut reader, mut body)!
	assert body.bytestr() == 'secure'

	path := if prefetch { '/ws' } else { '/ws?idle=1' }
	mut request := ('GET ${path} HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\n' +
		'Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n' +
		'Sec-WebSocket-Key: ${websocket_key}\r\n\r\n').bytes()
	first_frame := masked_frame(0x81, 'from client'.bytes())
	if prefetch {
		// A single TLS write puts the request and first frame into the parser's first read.
		request << first_frame
	}
	client.write(request)!
	headers := read_headers(mut reader)!
	assert headers[0] == 'HTTP/1.1 101 Switching Protocols'
	assert 'Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=' in headers
	if !prefetch {
		client.write(first_frame)!
	}
	for i, message in ['from client', 'second message'] {
		if i == 1 {
			client.write(masked_frame(0x81, message.bytes()))!
		}
		echo := 'echo: ' + message
		mut frame := []u8{len: 2 + echo.len}
		io.read_full(mut reader, mut frame)!
		assert frame[0] == 0x81
		assert frame[1] == u8(echo.len)
		assert frame[2..].bytestr() == echo
	}
	client.write(masked_frame(0x88, close_payload))!
	mut close_reply := []u8{len: 4}
	io.read_full(mut reader, mut close_reply)!
	assert close_reply == [u8(0x88), 0x02, close_payload[0], close_payload[1]]
	check_tls_eof(mut client)!
	closed = true
	client.close()!
	wait_for_websocket_worker(finished)!
}

fn check_tls_eof(mut client ssl.SSLConn) ! {
	mut tail := []u8{len: 1}
	if n := client.read(mut tail) {
		assert n == 0
	} else {
		assert err is io.Eof
	}
}

fn wait_for_websocket_worker(finished chan bool) ! {
	select {
		_ := <-finished {
		}
		5 * time.second {
			return error('taken-over TLS connection did not close')
		}
	}
}

fn check_failed_handshake(finished chan bool) ! {
	mut client := ssl.new_ssl_conn()!
	client.dial('127.0.0.1', port)!
	client.set_read_timeout(3 * time.second)
	client.write_string('GET /ws HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\n\r\n')!
	check_tls_eof(mut client)!
	client.close()!
	wait_for_websocket_worker(finished)!
}

fn test_wss_takeover_preserves_tls_and_prefetched_frames() ! {
	cert_dir := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')
	mut app := &App{}
	spawn veb.run_at[App, Context](mut app,
		host:               '127.0.0.1'
		port:               port
		family:             .ip
		timeout_in_seconds: 3
		ssl_config:         ssl.SSLConnectConfig{
			cert:     os.join_path(cert_dir, 'server.crt')
			cert_key: os.join_path(cert_dir, 'server.key')
		}.SSLConnectConfig
	)
	select {
		_ := <-app.started {
		}
		5 * time.second {
			return error('HTTPS server did not start')
		}
	}
	check_websocket(false, app.finished)!
	check_websocket(true, app.finished)!
	check_failed_handshake(app.finished)!
}
