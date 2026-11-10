module veb

import fasthttp
import net.http
import os

struct WorkerWriterContext {
	Context
}

@[heap]
struct WorkerWriterApp {
mut:
	retained []&WorkerWriterContext
	routes   map[string]Route
	active   int
	begins   int
	ends     int
}

fn (mut app WorkerWriterApp) before_handle_request(_req http.Request) {
	if app.active != 0 { panic('worker started overlapping dispatch') }
	app.active++
	app.begins++
}

fn (mut app WorkerWriterApp) after_handle_request(_req http.Request) {
	if app.active != 1 { panic('worker ended an inactive dispatch') }
	app.active--
	app.ends++
}

@['/:path...'; get]
fn (mut app WorkerWriterApp) index(mut ctx WorkerWriterContext, raw_path string) Result {
	if app.active != 1 { panic('handler ran outside request activity hooks') }
	path := raw_path.trim_left('/')
	app.retained << &ctx
	ctx.form['saved'] = path
	ctx.res.header.add(.set_cookie, 'first=' + path)
	ctx.res.header.add(.set_cookie, 'second=' + path)
	if path == 'file' {
		return ctx.file(os.join_path(os.dir(@FILE), 'tests', 'testdata', 'root.txt'))
	}
	if path == 'panic' { panic('worker writer panic fixture') }
	result := ctx.text('reply:' + path)
	if path == 'manual' { ctx.takeover_mode = .manual }
	if path == 'reusable' { ctx.takeover_mode = .reusable }
	return result
}

fn worker_writer_params(mut app WorkerWriterApp) RequestParams {
	app.routes = generate_routes[WorkerWriterApp, WorkerWriterContext](app) or { panic(err) }
	return RequestParams{ global_app: unsafe { voidptr(&app) }, routes: &app.routes }
}

fn worker_writer_append(params &RequestParams, state voidptr, path string, close bool) ([]u8, fasthttp.ResponseControl) {
	connection := if close { 'close' } else { 'keep-alive' }
	raw := 'GET /${path} HTTP/1.1\r\nHost: localhost\r\nConnection: ${connection}\r\n\r\n'
	mut out := []u8{}
	mut ctl := fasthttp.ResponseControl{}
	step := parallel_append_handler[WorkerWriterApp, WorkerWriterContext](fasthttp.HttpRequest{
		buffer:         raw.bytes()
		client_conn_fd: -1
		user_data:      unsafe { voidptr(params) }
	}, mut out, state, mut ctl)
	assert step == .done
	return out, ctl
}

fn assert_worker_writer_empty(state &ResponseWriterState) {
	assert !state.busy
	// Cleared storage deliberately has no initialized map headers; inspect raw
	// bytes instead of calling Context/map operations before the next request.
	data := unsafe { &u8(&state.context) }
	for i in 0 .. int(sizeof(Context)) {
		assert unsafe { data[i] } == 0
	}
}

fn test_worker_writer_reuse_preserves_escaped_application_contexts() {
	state_ptr := make_response_writer_state[WorkerWriterApp]()
	if !response_writer_state_enabled[WorkerWriterApp]() {
		assert state_ptr == unsafe { nil }
		return
	}
	state := unsafe { &ResponseWriterState(state_ptr) }
	mut app := &WorkerWriterApp{}
	params := worker_writer_params(mut app)
	for i in 0 .. 32 {
		path := 'request-${i}'
		out, ctl := worker_writer_append(&params, state_ptr, path, i % 2 == 1)
		assert ctl.should_close == (i % 2 == 1)
		wire := out.bytestr()
		assert wire.ends_with('reply:' + path)
		assert wire.count('Set-Cookie:') == 2
		assert_worker_writer_empty(state)
	}
	gc_collect()
	for i, ctx in app.retained {
		assert voidptr(ctx) != state_ptr
		assert ctx.req.url == '/request-${i}'
		assert ctx.form['saved'] == 'request-${i}'
		assert ctx.res.body == 'reply:request-${i}'
		assert ctx.res.header.values(.set_cookie) == ['first=request-${i}', 'second=request-${i}']
	}
}

fn test_worker_writer_clears_after_parse_errors_and_takeovers() {
	state_ptr := make_response_writer_state[WorkerWriterApp]()
	if state_ptr == unsafe { nil } { return }
	state := unsafe { &ResponseWriterState(state_ptr) }
	mut app := &WorkerWriterApp{}
	params := worker_writer_params(mut app)
	mut out := []u8{}
	mut ctl := fasthttp.ResponseControl{}
	step := parallel_append_handler[WorkerWriterApp, WorkerWriterContext](fasthttp.HttpRequest{
		buffer:    'bad request\r\n\r\n'.bytes()
		user_data: unsafe { voidptr(&params) }
	}, mut out, state_ptr, mut ctl)
	assert step == .close
	assert ctl.should_close
	assert_worker_writer_empty(state)
	// A valid HTTP head with invalid JSON returns a separate error Context.
	// The pool slot is unused here; freeing the error response must use identity,
	// never a structural comparison against the deliberately cleared slot.
	head := 'POST /normal HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/json\r\nContent-Length: 1\r\n\r\n'
	mut invalid_out := []u8{}
	mut invalid_ctl := fasthttp.ResponseControl{}
	invalid_step := parallel_append_handler[WorkerWriterApp, WorkerWriterContext](fasthttp.HttpRequest{
		buffer:    (head + '{').bytes()
		body:      fasthttp.Slice{ start: head.len, len: 1 }
		user_data: unsafe { voidptr(&params) }
	}, mut invalid_out, state_ptr, mut invalid_ctl)
	assert invalid_step == .done
	assert invalid_out.bytestr().starts_with('HTTP/1.1 400')
	assert_worker_writer_empty(state)
	panic_out, _ := worker_writer_append(&params, state_ptr, 'panic', false)
	assert panic_out.bytestr().starts_with('HTTP/1.1 500')
	assert_worker_writer_empty(state)
	for mode in ['manual', 'reusable', 'file', 'normal'] {
		wire, control := worker_writer_append(&params, state_ptr, mode, true)
		if mode == 'manual' {
			assert control.takeover_mode == .manual
		} else if mode == 'reusable' {
			assert control.takeover_mode == .reusable
		} else if mode == 'file' {
			assert control.file_path.ends_with('root.txt')
		} else {
			assert wire.bytestr().ends_with('reply:normal')
		}
		assert_worker_writer_empty(state)
	}
	gc_collect()
	for ctx in app.retained {
		assert ctx.req.url != ''
	}
}

fn test_busy_writer_slot_falls_back_without_overwriting_outer_request() {
	state_ptr := make_response_writer_state[WorkerWriterApp]()
	if state_ptr == unsafe { nil } { return }
	mut state := unsafe { &ResponseWriterState(state_ptr) }
	state.busy = true
	state.context.res.body = 'outer sentinel'
	mut app := &WorkerWriterApp{}
	params := worker_writer_params(mut app)
	out, _ := worker_writer_append(&params, state_ptr, 'nested', false)
	assert out.bytestr().ends_with('reply:nested')
	assert state.busy
	assert state.context.res.body == 'outer sentinel'
	state.busy = false
	state.context.res.body = ''
}

struct WorkerWriterControllerApp {
	Controller
}

fn test_controller_writer_storage_is_disabled() {
	assert !response_writer_state_enabled[WorkerWriterControllerApp]()
	assert make_response_writer_state[WorkerWriterControllerApp]() == unsafe { nil }
}

fn test_render_request_retains_independent_heap_snapshots() {
	mut app := &WorkerWriterApp{}
	first := render_request[WorkerWriterApp, WorkerWriterContext](mut app, http.Request{ url: '/first' }) or { panic(err) }
	second := render_request[WorkerWriterApp, WorkerWriterContext](mut app, http.Request{ url: '/second' }) or { panic(err) }
	gc_collect()
	assert voidptr(first) != voidptr(second)
	assert first.res.body == 'reply:first'
	assert second.res.body == 'reply:second'
	assert app.retained[0].res.body == 'reply:first'
}

fn worker_writer_job(label int) int {
	state := make_response_writer_state[WorkerWriterApp]()
	mut app := &WorkerWriterApp{}
	params := worker_writer_params(mut app)
	for i in 0 .. 32 {
		path := 'worker-${label}-${i}'
		out, _ := worker_writer_append(&params, state, path, false)
		assert out.bytestr().ends_with('reply:' + path)
	}
	return app.retained.len
}

fn test_worker_writer_threads_keep_request_state_independent() {
	mut jobs := []thread int{}
	for i in 0 .. 4 { jobs << spawn worker_writer_job(i) }
	for job in jobs {
		assert job.wait() == 32
	}
}

// Explicit checks also exercise slot reuse and retained contexts under -prod.
fn test_worker_writer_contract_in_production_builds() {
	state_ptr := make_response_writer_state[WorkerWriterApp]()
	mut app := &WorkerWriterApp{}
	params := worker_writer_params(mut app)
	for i in 0 .. 16 {
		path := 'production-${i}'
		out, ctl := worker_writer_append(&params, state_ptr, path, i % 2 == 1)
		if !out.bytestr().ends_with('reply:' + path) || ctl.should_close != (i % 2 == 1) {
			panic('worker writer returned the wrong response')
		}
	}
	if app.active != 0 || app.begins != 16 || app.ends != 16 {
		panic('worker writer did not balance request activity hooks')
	}
	gc_collect()
	for i, ctx in app.retained {
		if ctx.req.url != '/production-${i}' || ctx.res.body != 'reply:production-${i}' {
			panic('worker writer overwrote a retained application context')
		}
	}
	if state_ptr != unsafe { nil } {
		state := unsafe { &ResponseWriterState(state_ptr) }
		if state.busy || state.context.res.body != '' {
			panic('worker writer retained request state')
		}
	}
}
