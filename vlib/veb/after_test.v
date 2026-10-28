module veb

import net
import net.http
import time
import fasthttp

struct AfterApp {}

struct AfterContext {
	Context
}

fn after_answer(mut ctx Context, arg u64) {
	ctx.text('late:${arg}')
}

fn after_chain(mut ctx Context, arg u64) {
	if arg == 0 {
		ctx.after(0, after_chain, 1)
		return
	}
	ctx.text('chained:${arg}')
}

@['/delay']
fn (mut app AfterApp) delay(mut ctx AfterContext) Result {
	return ctx.after(400, after_answer, 7)
}

@['/chain']
fn (mut app AfterApp) chain(mut ctx AfterContext) Result {
	return ctx.after(150, after_chain_live, 0)
}

@['/immediate']
fn (mut app AfterApp) immediate(mut ctx AfterContext) Result {
	return ctx.after(0, after_chain, 0)
}

fn after_chain_live(mut ctx Context, arg u64) {
	if arg == 0 {
		ctx.after(150, after_chain_live, 1)
		return
	}
	ctx.text('chained:${arg}')
}

fn after_inline_ctx() Context {
	return Context{
		req:   http.Request{
			method: .get
			url:    '/delayed'
			header: http.new_header()
		}
		query: {}
		form:  {}
		files: {}
		res:   http.Response{
			header: http.new_header()
		}
	}
}

fn test_after_without_transport_answers_inline() {
	mut ctx := after_inline_ctx()
	ctx.after(0, after_answer, 7)
	assert ctx.done
	assert ctx.takeover_mode == .none
	assert ctx.res.body == 'late:7'
	assert ctx.res.status_code == int(http.Status.ok)
}

fn test_after_without_transport_waits_at_least_ms() {
	mut ctx := after_inline_ctx()
	start := time.ticks()
	ctx.after(150, after_answer, 1)
	elapsed := time.ticks() - start
	assert elapsed >= 120, 'deferred response answered after ${elapsed}ms, want >= 120ms'
	assert ctx.res.body == 'late:1'
}

fn test_after_without_transport_treats_nonpositive_delay_as_now() {
	mut ctx := after_inline_ctx()
	ctx.after(-5, after_answer, 2)
	assert ctx.done
	assert ctx.res.body == 'late:2'
}

fn test_after_without_transport_supports_rearming() {
	mut ctx := after_inline_ctx()
	ctx.after(0, after_chain, 0)
	assert ctx.done
	assert ctx.res.body == 'chained:1'
}

// after_live_pair returns a connected loopback pair: the server fd for veb's
// timer thread to own, the client end for the test to read, and the listener
// for the caller to close. Nothing here touches the test prohibited paths:
// localhost TCP works on every OS veb tests run on.
fn after_live_pair() !(int, &net.TcpConn, &net.TcpListener) {
	mut ln := net.listen_tcp(.ip, '127.0.0.1:0')!
	addr := ln.addr()!.str()
	ch := chan &net.TcpConn{cap: 1}
	spawn fn (dial_addr string, dialed chan &net.TcpConn) {
		conn := net.dial_tcp(dial_addr) or { panic('dial ${dial_addr}: ${err}') }
		dialed <- conn
	}(addr, ch)
	mut srv := ln.accept()!
	fd := srv.handle
	// Ownership of the accepted socket passes to veb's timer thread through
	// the fd; this wrapper is intentionally never closed.
	cli := <-ch
	return fd, cli, ln
}

fn after_live_request(fd int) &Context {
	mut app := &AfterApp{}
	routes := generate_routes[AfterApp, AfterContext](app) or { panic(err) }
	params := RequestParams{
		routes: &routes
	}
	return handle_request_and_route[AfterApp, AfterContext](mut app, http.Request{
		method: .get
		url:    '/delay'
		header: http.new_header_from_map({
			http.CommonHeader.host: '127.0.0.1'
		})
	}, fd, params)
}

fn after_read_until(mut cli net.TcpConn, marker string) string {
	// The head and the body can arrive in separate segments: keep reading
	// until the marker (the end of the body) is in, or the peer goes away.
	mut raw := ''
	mut buf := []u8{len: 4096}
	for _ in 0 .. 16 {
		n := cli.read(mut buf) or { break }
		if n <= 0 {
			break
		}
		raw += buf[..n].bytestr()
		if raw.contains(marker) {
			break
		}
	}
	return raw
}

fn test_after_live_transport_returns_immediately_and_answers_late() {
	fd, mut cli, mut ln := after_live_pair()!
	defer {
		ln.close() or {}
		cli.close() or {}
	}
	cli.set_read_timeout(10 * time.second)
	start := time.ticks()
	completed := after_live_request(fd)
	returned_in := time.ticks() - start
	assert returned_in < 250, 'handler held the worker for ${returned_in}ms'
	assert completed.takeover_mode == .manual

	read_start := time.ticks()
	resp := after_read_until(mut cli, 'late:7')
	waited := time.ticks() - read_start
	assert waited >= 350, 'deferred response arrived after ${waited}ms, want >= 350ms'
	assert resp.contains('late:7'), 'unexpected deferred response: ${resp}'

	// The timer closes the connection after the single deferred response.
	eof_start := time.ticks()
	mut eof_buf := []u8{len: 64}
	n := cli.read(mut eof_buf) or { 0 }
	assert n == 0, 'connection should be closed after the deferred response'
	assert time.ticks() - eof_start < 2000, 'EOF took too long: the connection was not closed'
}

fn test_after_live_transport_supports_rearming() {
	fd, mut cli, mut ln := after_live_pair()!
	defer {
		ln.close() or {}
		cli.close() or {}
	}
	cli.set_read_timeout(10 * time.second)
	mut app := &AfterApp{}
	routes := generate_routes[AfterApp, AfterContext](app) or { panic(err) }
	params := RequestParams{
		routes: &routes
	}
	start := time.ticks()
	completed := handle_request_and_route[AfterApp, AfterContext](mut app, http.Request{
		method: .get
		url:    '/chain'
		header: http.new_header_from_map({
			http.CommonHeader.host: '127.0.0.1'
		})
	}, fd, params)
	assert time.ticks() - start < 250, 'handler held the worker'
	assert completed.takeover_mode == .manual

	resp := after_read_until(mut cli, 'chained:1')
	assert time.ticks() - start >= 250, 're-armed response arrived too early'
	assert resp.contains('chained:1'), 'unexpected re-armed response: ${resp}'
}

fn test_after_append_handler_zero_delay_rearming_owns_request_arena() {
	mut app := &AfterApp{}
	routes := generate_routes[AfterApp, AfterContext](app)!
	params := &RequestParams{
		global_app: voidptr(app)
		routes:     &routes
	}
	raw := 'GET /immediate HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n'.bytes()
	for _ in 0 .. 16 {
		fd, mut cli, mut ln := after_live_pair()!
		cli.set_read_timeout(10 * time.second)
		mut out := []u8{}
		mut ctl := fasthttp.ResponseControl{}
		step := parallel_append_handler[AfterApp, AfterContext](fasthttp.HttpRequest{
			buffer:         raw
			client_conn_fd: fd
			user_data:      voidptr(params)
		}, mut out, unsafe { nil }, mut ctl)
		assert step == .done
		assert ctl.takeover_mode == .manual
		assert out.len == 0
		resp := after_read_until(mut cli, 'chained:1')
		assert resp.count('HTTP/1.1') == 1, resp
		assert resp.contains('chained:1'), resp
		mut eof_buf := []u8{len: 64}
		assert cli.read(mut eof_buf) or { 0 } == 0
		cli.close() or {}
		ln.close() or {}
	}
}
