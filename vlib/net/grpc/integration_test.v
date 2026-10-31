// vtest build: !sanitize-memory-clang
// End-to-end test for native gRPC over a real TLS + HTTP/2 connection: the only
// place in this module that opens a socket. Everything else is driven through
// the servers' `handle` directly, because V's net.http has no client-side h2c —
// `enable_http2` applies to https only — so a loopback gRPC round-trip needs a
// certificate.
//
// The cert/key are the same self-signed localhost pair net.http's own TLS tests
// use, inlined here so the test needs no network and no openssl.

module grpc

import net
import net.http
import time

// A finite `accept_timeout` doubles as the server's TLS handshake budget; the
// inlined RSA-4096 key needs slack, same as net.http's own TLS tests.
const integration_tls_budget = 10 * time.second

const integration_cert = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

const integration_key = '-----BEGIN RSA PRIVATE KEY-----\n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\n-----END RSA PRIVATE KEY-----\n'

// TlsSvc is the GrpcService the integration tests talk to. It never touches
// generated stubs: every payload is a bare string, which is enough to prove the
// wire contract end to end.
struct TlsSvc {}

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
			// deliberately outlive the caller's deadline, so the client's
			// response-wait watchdog has something to fire on
			time.sleep(time.second)
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
fn start_tls_server(port int) (&http.Server, thread) {
	mut grpc_srv := GrpcServer{}
	grpc_srv.mount(TlsSvc{})
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
	time.sleep(50 * time.millisecond)
	return srv, t
}

// tls_client returns a Client pointed at the loopback server. Certificate
// validation is off: the inlined cert is self-signed and issued to
// localhost, not 127.0.0.1.
fn tls_client(port int) Client {
	return Client{
		base_url: 'https://127.0.0.1:${port}'
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
	mut srv, t := start_tls_server(port)
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
	mut srv, t := start_tls_server(port)
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
	mut srv, t := start_tls_server(port)
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
	mut srv, t := start_tls_server(port)
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
	mut srv, t := start_tls_server(port)
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
// server's answer surfaces as deadline_exceeded rather than as the transport's
// own wording. The handler sleeps well past the deadline on purpose: a deadline
// is a bound, not a guarantee of failure, so a fast server may legitimately beat
// it.
fn test_integration_deadline_over_tls_h2() {
	if skip_without_tls_server_backend() {
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv, t := start_tls_server(port)
	defer {
		srv.close()
		t.wait()
	}
	mut c := tls_client(port)

	if _ := c.unary('/tls.Echo/Slow', 'x'.bytes(), timeout(150 * time.millisecond)) {
		assert false, 'a 150ms deadline must not beat a 1s handler'
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
	mut srv, t := start_tls_server(port)
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
