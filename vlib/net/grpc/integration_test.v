// vtest build: !sanitize-memory-clang
// End-to-end test for native gRPC over a real TLS + HTTP/2 connection: the only
// place in this module that opens a socket. Everything else is driven through
// the servers' `handle` directly, because V's net.http has no client-side h2c —
// `enable_http2` applies to https only — so a loopback gRPC round-trip needs a
// certificate.
//
// The cert/key are embedded from the same untrusted localhost fixture the HTTP tests use.

module grpc

import net
import net.http
import time

// A finite `accept_timeout` doubles as the server's TLS handshake budget.
// Allow time for handshakes under concurrent test load.
const integration_tls_budget = 10 * time.second

fn testsuite_end() {
	// fetch() keeps TLS connections in the default HTTP transport after a call.
	http.close_idle_connections()
}

const integration_cert = $embed_file('../testdata/localhost_test_cert.pem').to_string()

const integration_key = $embed_file('../testdata/localhost_test_key.pem').to_string()

// TlsSvc is the GrpcService the integration tests talk to. It never touches
// generated stubs: every payload is a bare string, which is enough to prove the
// wire contract end to end.
struct TlsSvc {
	release_slow_call chan bool
}

fn (mut s TlsSvc) grpc_call(path string, reqs [][]u8, mut ctx ServerContext) !([][]u8, bool) {
	match path {
		'/tls.Echo/Unary' {
			ctx.set_header('x-lead', 'L')
			ctx.set_trailer('x-trail', 'T')
			return [reqs[0]], true
		}
		'/tls.Echo/Fan' {
			return [reqs[0], reqs[0]], true
		}
		'/tls.Echo/Boom' {
			return StatusError{
				status: Status{
					code:    .invalid_argument
					message: 'no good 🚀'
				}
			}
		}
		'/tls.Echo/Slow' {
			// Wait for the deadline check, with a fallback if the client misses it.
			select {
				_ := <-s.release_slow_call {
				}
				1 * time.second {
				}
			}
			return [reqs[0]], true
		}
		else {
			return [][]u8{}, false
		}
	}
}

// pick_port reserves an ephemeral loopback port and hands the number back, so
// the server binds it a moment later.
fn pick_port() !int {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := l.addr()!.port()!
	l.close()!
	return port
}

// start_tls_server brings up a GrpcServer on loopback TLS with HTTP/2 enabled,
// already running when it returns. `tls_budget` bounds the handshake.
fn start_tls_server(port int, service TlsSvc) (&http.Server, thread) {
	mut grpc_srv := GrpcServer{}
	grpc_srv.mount(service)
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   integration_cert
		cert_key:               integration_key
		in_memory_verification: true
		accept_timeout:         integration_tls_budget
		enable_http2:           true
		handler:                grpc_srv
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		panic('integration gRPC server failed to start: ${err}')
	}
	return srv, t
}

// tls_client returns a Client pointed at the loopback server. Certificate
// validation is off because the fixture's issuer is untrusted.
fn tls_client(port int) Client {
	return Client{
		base_url: 'https://127.0.0.1:${port}'
		validate: false
	}
}

// skip_without_tls_server_backend bails out of a test when TLS termination is
// unavailable, which is the case for the OpenSSL backend.
fn skip_without_tls_server_backend() bool {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return true
	}
	return false
}

// test_integration_unary_over_tls_h2 is the headline case: one unary RPC across
// a real TLS connection, HTTP/2 negotiated over ALPN, with the status arriving
// in HTTP/2 trailers rather than headers.
fn test_integration_unary_over_tls_h2() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv, t := start_tls_server(port, TlsSvc{})
	defer {
		srv.close()
		t.wait()
	}
	mut c := tls_client(port)

	reply := c.unary('/tls.Echo/Unary', 'ping'.bytes()) or {
		assert false, 'unary failed: ${err}'
		return
	}
	assert reply.payload == 'ping'.bytes()
	// leading metadata came back as a response header; the h2 layer folds
	// trailers into the same header set, so trailing metadata is visible too
	assert reply.metadata['x-lead'] == ['L']
	assert reply.metadata['x-trail'] == ['T']
	// the gRPC control headers are consumed by the client, never leaked to
	// the caller as metadata
	assert 'grpc-status' !in reply.metadata
	assert 'grpc-message' !in reply.metadata
}

// test_integration_default_validate_rejects_untrusted_server checks the
// default: Client.validate is true, so the untrusted loopback certificate is
// rejected unless the caller opts out. tls_client sets validate: false.
fn test_integration_default_validate_rejects_untrusted_server() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv, t := start_tls_server(port, TlsSvc{})
	defer {
		srv.close()
		t.wait()
	}
	mut c := Client{
		base_url: 'https://127.0.0.1:${port}'
	}
	if _ := c.unary('/tls.Echo/Unary', 'ping'.bytes()) {
		assert false, 'default validate must reject the untrusted server'
	}
}

// test_integration_response_is_http2 pins that the negotiated protocol really
// was h2: without `enable_http2` in do_call, this call silently degrades to
// HTTP/1.1 and no conforming server would answer it.
fn test_integration_server_stream_over_tls_h2() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv, t := start_tls_server(port, TlsSvc{})
	defer {
		srv.close()
		t.wait()
	}
	mut c := tls_client(port)

	reply := c.server_stream('/tls.Echo/Fan', 'tick'.bytes()) or {
		assert false, 'server_stream failed: ${err}'
		return
	}
	assert reply.payloads.len == 2
	assert reply.payloads[0] == 'tick'.bytes()
	assert reply.payloads[1] == 'tick'.bytes()
}

// test_integration_client_stream_over_tls_h2 sends several framed request
// messages in one POST and reads back the single reply.
fn test_integration_client_stream_over_tls_h2() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv, t := start_tls_server(port, TlsSvc{})
	defer {
		srv.close()
		t.wait()
	}
	mut c := tls_client(port)

	reply := c.client_stream('/tls.Echo/Unary', ['one'.bytes(), 'two'.bytes()]) or {
		assert false, 'client_stream failed: ${err}'
		return
	}
	// the stub echoes the first message back; the point is that every request
	// frame crossed the h2 connection without tripping the framing code
	assert reply.payload == 'one'.bytes()
}

// test_integration_error_status_over_tls_h2 checks the failure path end to end:
// the server answers HTTP 200 with a Trailers-Only status block, and the client
// turns that into a StatusError with the message percent-decoded.
fn test_integration_error_status_over_tls_h2() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv, t := start_tls_server(port, TlsSvc{})
	defer {
		srv.close()
		t.wait()
	}
	mut c := tls_client(port)

	if _ := c.unary('/tls.Echo/Boom', 'x'.bytes()) {
		assert false, 'a handler error must not look like success'
	} else {
		assert err is StatusError, 'expected StatusError, got ${err.msg()}'
		e := err as StatusError
		assert e.status.code == .invalid_argument
		assert e.status.message == 'no good 🚀'
	}
}

// test_integration_unknown_procedure_over_tls_h2 pins the routing miss.
fn test_integration_unknown_procedure_over_tls_h2() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv, t := start_tls_server(port, TlsSvc{})
	defer {
		srv.close()
		t.wait()
	}
	mut c := tls_client(port)

	if _ := c.unary('/tls.Echo/Missing', 'x'.bytes()) {
		assert false, 'a routing miss must not look like success'
	} else {
		assert err is StatusError
		e := err as StatusError
		assert e.status.code == .unimplemented
	}
}

// test_integration_deadline_over_tls_h2 checks that a deadline shorter than the
// server's answer surfaces as deadline_exceeded. The test holds the handler
// until the client returns to ensure the response stays pending.
fn test_integration_deadline_over_tls_h2() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	release_slow_call := chan bool{}
	mut srv, t := start_tls_server(port, TlsSvc{ release_slow_call: release_slow_call })
	defer {
		release_slow_call.close()
		srv.close()
		t.wait()
	}
	mut c := tls_client(port)

	if _ := c.unary('/tls.Echo/Slow', 'x'.bytes(), timeout(150 * time.millisecond)) {
		assert false, 'expected the 150ms deadline to expire while the handler waited'
	} else {
		assert err is StatusError, 'expected StatusError, got ${err.msg()}'
		e := err as StatusError
		assert e.status.code == .deadline_exceeded, 'got ${e.status.code}: ${e.status.message}'
	}
}

// test_integration_client_requires_http2 is the regression test for the port's
// main fix: a bare http.fetch to the same listener, without enable_http2, gets
// an HTTP/1.1 response that carries no grpc-status at all. That is exactly the
// silent degradation grpc.Client avoids by opting in.
fn test_integration_client_requires_http2() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv, t := start_tls_server(port, TlsSvc{})
	defer {
		srv.close()
		t.wait()
	}
	mut h := http.new_header()
	h.add(.content_type, 'application/grpc+proto')
	h.add_custom('te', 'trailers') or {}
	mut body := encode_frame('x'.bytes(), false)

	// HTTP/1.1: the h2-only trailers never arrive, so no grpc-status exists
	resp := http.fetch(
		url:      'https://127.0.0.1:${port}/tls.Echo/Unary'
		method:   .post
		header:   h
		data:     body.bytestr()
		validate: false
	) or {
		assert false, 'h1 fetch failed: ${err}'
		return
	}
	assert resp.version() == .v1_1
	assert resp.header.get_custom('grpc-status') == none
}
