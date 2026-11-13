module veb

import fasthttp

struct ResponseWriterWireContext {
	Context
}

@[heap]
struct ResponseWriterWireApp {
mut:
	retained string
}

fn response_writer_wire_payload() string { return 'wire\x00payload'.repeat(2048) }

fn (mut app ResponseWriterWireApp) index(mut ctx ResponseWriterWireContext) Result {
	ctx.res.header.add(.set_cookie, 'first=1')
	ctx.res.header.add(.set_cookie, 'second=2')
	result := ctx.text(response_writer_wire_payload())
	app.retained = ctx.res.body
	return result
}

fn test_append_writer_serializes_then_releases_context_without_invalidating_alias() {
	mut app := &ResponseWriterWireApp{}
	routes := generate_routes[ResponseWriterWireApp, ResponseWriterWireContext](app) or { panic(err) }
	controllers := check_duplicate_routes_in_controllers[ResponseWriterWireApp](app, routes) or { panic(err) }
	params := RequestParams{ global_app: unsafe { voidptr(app) }, routes: &routes, controllers_sorted: controllers }
	mut out := []u8{cap: 65536}
	for close in [false, true] {
		connection := if close { 'close' } else { 'keep-alive' }
		raw := 'GET / HTTP/1.1\r\nHost: localhost\r\nConnection: ${connection}\r\n\r\n'
		mut control := fasthttp.ResponseControl{}
		out.clear()
		step := parallel_append_handler[ResponseWriterWireApp, ResponseWriterWireContext](fasthttp.HttpRequest{
			buffer:         raw.bytes()
			client_conn_fd: -1
			user_data:      unsafe { voidptr(&params) }
		}, mut out, unsafe { nil }, mut control)
		assert step == .done
		assert control.should_close == close
		assert control.takeover_mode == .none
		wire := out.bytestr()
		assert wire.starts_with('HTTP/1.1 200 OK\r\n')
		assert wire.count('Set-Cookie:') == 2
		assert wire.ends_with(response_writer_wire_payload())
		mut churn := []string{}
		for _ in 0 .. 256 { churn << 'reuse'.repeat(4096) }
		gc_collect()
		assert app.retained == response_writer_wire_payload()
	}
}
