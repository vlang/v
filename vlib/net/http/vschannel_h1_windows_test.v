module http

import net
import net.mbedtls
import time

// Regression coverage for vlang/v#27705: the one-shot vschannel HTTP/1.1
// client (vschannel_h1_do/vschannel_h1_on_open, thirdparty/vschannel/
// vschannel.c) used to have no way to know a response was complete other
// than the peer closing the connection or sending a TLS close_notify. HTTP/
// 1.1 keep-alive is the default -- a compliant server is free to leave the
// connection open after responding -- so a peer that does that stalled the
// client for as long as the peer chose to keep the socket open.

const vh1t_cert_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const vh1t_key_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'
const vh1t_body = 'hello from a keep-alive peer'

fn vh1t_serve_content_length(mut listener mbedtls.SSLListener, suffix string, request_done chan bool) !bool {
	mut conn := listener.accept_with_timeout(5 * time.second)!
	defer {
		conn.shutdown() or {}
	}
	vh1t_read_request(mut conn)!
	conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: ${vh1t_body.len}${suffix}\r\n\r\n${vh1t_body}')!
	// Keep the peer open until the client returns. If the client waits for EOF,
	// close after the deadline and report that the server had to close first.
	select {
		_ := <-request_done {
			return true
		}
		3 * time.second {
		}
	}
	return false
}

fn vh1t_check_content_length(suffix string) ! {
	port, mut listener := vh1t_start_listener()!
	defer {
		listener.shutdown() or {}
	}
	request_done := chan bool{cap: 1}
	th := spawn vh1t_serve_content_length(mut listener, suffix, request_done)
	resp := vh1t_fetch(port) or {
		request_done <- true
		th.wait() or {}
		return err
	}
	request_done <- true
	client_finished_first := th.wait()!
	assert resp.status_code == 200
	assert resp.body == vh1t_body
	assert client_finished_first, 'client waited for the keep-alive peer to close after receiving Content-Length bytes'
}

fn test_vschannel_h1_one_shot_does_not_wait_for_keep_alive_peer_to_close() {
	vh1t_check_content_length('')!
}

fn test_vschannel_h1_content_length_with_trailing_whitespace_still_frames() {
	// The completion detector must allow optional whitespace after the length.
	vh1t_check_content_length(' \t')!
}

fn vh1t_start_listener() !(int, &mbedtls.SSLListener) {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     vh1t_cert_path
		cert_key: vh1t_key_path
		validate: false
	})!
	return port, listener
}

fn vh1t_fetch(port int) !Response {
	return fetch(
		url:                      'https://127.0.0.1:${port}/'
		validate:                 false
		max_retries:              1
		enable_http2:             false
		disable_connection_reuse: true
	)
}

fn vh1t_read_request(mut conn mbedtls.SSLConn) ! {
	conn.set_read_timeout(10 * time.second)
	mut buf := []u8{len: 4096}
	mut sofar := []u8{}
	for !sofar.bytestr().contains('\r\n\r\n') {
		n := conn.read(mut buf)!
		if n <= 0 {
			return error('peer closed before sending request headers')
		}
		sofar << buf[..n]
	}
}

const vh1t_te_ext_early = '0\r\n\r\n'
const vh1t_te_ext_rest = 'REST-AFTER-FAKE-CHUNK-TERMINATOR'

// vh1t_serve_xchunked responds with `Transfer-Encoding: xchunked` -- an
// extension coding whose NAME merely contains the substring "chunked" -- and
// a raw body whose first bytes happen to look like a complete chunked
// terminator, with the remainder arriving shortly after. A client that
// matches transfer codings by substring instead of by comma-separated token
// (the way net.http's own parse_response/has_header_token does) wrongly
// applies chunk framing, sees the fake terminator, and stops reading --
// silently truncating the body. Unknown codings must fall back to
// read-until-close; the server closes right after the second write, so the
// correct path stays fast.
fn vh1t_serve_xchunked(mut listener mbedtls.SSLListener) ! {
	mut conn := listener.accept_with_timeout(5 * time.second)!
	defer {
		conn.shutdown() or {}
	}
	vh1t_read_request(mut conn)!
	conn.write_string('HTTP/1.1 200 OK\r\nTransfer-Encoding: xchunked\r\n\r\n${vh1t_te_ext_early}')!
	time.sleep(400 * time.millisecond)
	conn.write_string(vh1t_te_ext_rest)!
}

fn test_vschannel_h1_transfer_encoding_extension_token_is_not_chunked() ! {
	port, mut listener := vh1t_start_listener()!
	defer {
		listener.shutdown() or {}
	}
	th := spawn vh1t_serve_xchunked(mut listener)
	resp := vh1t_fetch(port) or {
		th.wait() or {}
		return err
	}
	th.wait()!

	assert resp.status_code == 200
	// parse_response leaves an unknown transfer coding's body raw (its own
	// has_header_token only matches real comma-separated `chunked` tokens),
	// so the full raw bytes -- including everything after the fake chunk
	// terminator -- must survive to here.
	assert resp.body == vh1t_te_ext_early + vh1t_te_ext_rest, 'body truncated at a fake chunk terminator inside a non-chunked (extension-coded) response'
}
