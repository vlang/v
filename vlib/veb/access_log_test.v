// access_log_test.v - one JSON line per response, and nothing else changes.
//
// An access log answers "what did the server do" after the fact: method,
// path, status, bytes, latency and request id, as JSON, on stdout, where
// collectors expect it. The format is a pure function of the finished
// context, so every field is asserted without a server, a clock or stdout;
// one render test then proves the pair lets the request through untouched.
module veb

import net.http
import time

struct LogApp {
	Middleware[LogContext]
}

struct LogContext {
	Context
}

@['/page']
fn (app &LogApp) page(mut ctx LogContext) Result {
	return ctx.text('page')
}

// log_ctx builds a finished Context with explicit fields, so the format is
// pinned without depending on a clock.
fn log_ctx(method http.Method, url string, status int, body string, start_ms i64, request_id string) Context {
	mut ctx := Context{
		req:   http.Request{
			method: method
			url:    url
			header: http.new_header()
		}
		query: {}
		form:  {}
		files: {}
		res:   http.Response{
			header: http.new_header()
		}
	}
	ctx.res.status_code = status
	ctx.res.body = body
	ctx.access_log_start_ms = start_ms
	if request_id != '' {
		ctx.res.header.set_custom('X-Request-ID', request_id) or { panic(err) }
	}
	return ctx
}

fn test_access_log_line_carries_method_path_status_bytes() {
	// `start_ms` is zero, so latency is the unknown marker -1; the shape is
	// what this pins, and the latency test below pins the arithmetic.
	mut ctx := log_ctx(.get, '/page', 200, 'page', 0, '')
	assert format_access_log(AccessLogOptions{}, mut ctx) == '{"method":"GET","path":"/page","status":200,"bytes":4,"latency_ms":-1,"request_id":""}'
}

fn test_access_log_latency_is_end_minus_start() {
	// The one assertion that touches the real clock: a stamp taken just now
	// must produce a small non-negative latency, never the -1 unknown.
	mut ctx := log_ctx(.get, '/page', 200, 'page', 0, '')
	ctx.access_log_start_ms = time.now().unix_milli()
	line := format_access_log(AccessLogOptions{}, mut ctx)
	assert line.contains('"latency_ms":')
	assert !line.contains('"latency_ms":-1')
}

fn test_access_log_line_drops_the_query_string_by_default() {
	// Query strings carry tokens and secrets; they must not reach the log
	// unless the operator asks for them.
	mut ctx := log_ctx(.get, '/search?q=secret&page=2', 200, 'ok', 0, '')
	assert format_access_log(AccessLogOptions{}, mut ctx) == '{"method":"GET","path":"/search","status":200,"bytes":2,"latency_ms":-1,"request_id":""}'
}

fn test_access_log_line_keeps_the_query_string_when_asked() {
	mut ctx := log_ctx(.get, '/search?q=v', 200, 'ok', 0, '')
	line := format_access_log(AccessLogOptions{
		include_query: true
	}, mut ctx)
	assert line.contains('"path":"/search?q=v"')
}

fn test_access_log_line_escapes_a_hostile_path() {
	// A path containing a quote and a newline must not break the JSON line or
	// smuggle a second line into the log.
	mut ctx := log_ctx(.get, '/x"y\nz', 404, '', 0, '')
	line := format_access_log(AccessLogOptions{}, mut ctx)
	assert line == '{"method":"GET","path":"/x\\"y\\nz","status":404,"bytes":0,"latency_ms":-1,"request_id":""}'
	assert !line.contains('\n')
}

fn test_access_log_line_carries_the_request_id() {
	mut ctx := log_ctx(.post, '/api', 201, 'created', 0, 'req-42')
	line := format_access_log(AccessLogOptions{}, mut ctx)
	assert line.contains('"request_id":"req-42"')
	assert line.contains('"status":201')
}

fn test_access_log_pair_lets_the_request_through() {
	// The pair must not answer or stop the chain: the handler still runs and
	// its body and status reach the client. What the finish half prints goes
	// to stdout, which this test does not capture; the format tests above own
	// the bytes.
	log := access_log[LogContext]()
	mut app := LogApp{}
	app.use(log.stamp())
	app.use(log.finish())
	ctx := render_request[LogApp, LogContext](mut app, http.Request{
		method: .get
		url:    '/page'
	}) or { panic(err) }
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'page'
	assert ctx.access_log_start_ms > 0
}
