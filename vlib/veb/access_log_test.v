// access_log_test.v - one JSON line per response, and nothing else changes.
//
// An access log answers "what did the server do" after the fact: method,
// path, status, bytes, latency and request id, as JSON, on stdout, where
// collectors expect it. The format is a pure function of the finished
// context, so every field is asserted without a server, a clock or stdout;
// render tests then prove the pair lets the request through untouched and
// logs what the client really got.
module veb

import encoding.utf8
import net.http
import os
import time

struct LogApp {
	Middleware[LogContext]
	file_path string
}

struct LogContext {
	Context
}

@['/page']
fn (app &LogApp) page(mut ctx LogContext) Result {
	return ctx.text('page')
}

@['/download']
fn (app &LogApp) download(mut ctx LogContext) Result {
	return ctx.file(app.file_path)
}

// LogSink collects the lines the finish half writes, in place of stdout.
struct LogSink {
mut:
	lines []string
}

fn (sink &LogSink) writer() fn (string) {
	return fn [sink] (line string) {
		unsafe {
			sink.lines << line
		}
	}
}

// use_access_log_with_query registers a pair built in this frame, which is
// gone by the time a request runs.
fn use_access_log_with_query(mut app LogApp, sink &LogSink) {
	log := AccessLog[LogContext]{
		options: AccessLogOptions{
			include_query: true
		}
		write:   sink.writer()
	}
	app.use(log.stamp())
	app.use(log.finish())
}

// log_ctx builds a finished Context with explicit fields, so the format is
// pinned without depending on a clock.
fn log_ctx(method http.Method, url string, status int, body string, start_ns i64, request_id string) Context {
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
	ctx.access_log_start_ns = start_ns
	if request_id != '' {
		ctx.res.header.set_custom('X-Request-ID', request_id) or { panic(err) }
	}
	return ctx
}

fn test_access_log_line_carries_method_path_status_bytes() {
	// `start_ns` is zero, so latency is the unknown marker -1; the shape is
	// what this pins, and the latency test below pins the arithmetic.
	ctx := log_ctx(.get, '/page', 200, 'page', 0, '')
	assert format_access_log(AccessLogOptions{}, &ctx) == '{"method":"GET","path":"/page","status":200,"bytes":4,"latency_ms":-1,"request_id":""}'
}

fn test_access_log_latency_is_end_minus_start() {
	// The one assertion that touches the real clock: a stamp taken just now
	// must produce a small non-negative latency, never the -1 unknown.
	mut ctx := log_ctx(.get, '/page', 200, 'page', 0, '')
	ctx.access_log_start_ns = i64(time.sys_mono_now())
	line := format_access_log(AccessLogOptions{}, &ctx)
	assert line.contains('"latency_ms":')
	assert !line.contains('"latency_ms":-1')
}

fn test_access_log_line_drops_the_query_string_by_default() {
	// Query strings carry tokens and secrets; they must not reach the log
	// unless the operator asks for them.
	ctx := log_ctx(.get, '/search?q=secret&page=2', 200, 'ok', 0, '')
	assert format_access_log(AccessLogOptions{}, &ctx) == '{"method":"GET","path":"/search","status":200,"bytes":2,"latency_ms":-1,"request_id":""}'
}

fn test_access_log_line_keeps_the_query_string_when_asked() {
	ctx := log_ctx(.get, '/search?q=v', 200, 'ok', 0, '')
	line := format_access_log(AccessLogOptions{
		include_query: true
	}, &ctx)
	assert line.contains('"path":"/search?q=v"')
}

fn test_access_log_line_escapes_a_hostile_path() {
	// A path containing a quote and a newline must not break the JSON line or
	// smuggle a second line into the log.
	ctx := log_ctx(.get, '/x"y\nz', 404, '', 0, '')
	line := format_access_log(AccessLogOptions{}, &ctx)
	assert line == '{"method":"GET","path":"/x\\"y\\nz","status":404,"bytes":0,"latency_ms":-1,"request_id":""}'
	assert !line.contains('\n')
}

fn test_access_log_line_carries_the_request_id() {
	ctx := log_ctx(.post, '/api', 201, 'created', 0, 'req-42')
	line := format_access_log(AccessLogOptions{}, &ctx)
	assert line.contains('"request_id":"req-42"')
	assert line.contains('"status":201')
}

fn test_access_log_line_reads_the_configured_request_id_header() {
	// request_id lets the header be renamed; the log has to follow it.
	mut ctx := log_ctx(.get, '/api', 200, 'ok', 0, 'req-42')
	ctx.res.header.set_custom('X-Trace-ID', 'trace-7') or { panic(err) }
	line := format_access_log(AccessLogOptions{
		request_id_header: 'X-Trace-ID'
	}, &ctx)
	assert line.contains('"request_id":"trace-7"')
}

fn test_access_log_line_replaces_bytes_that_are_not_utf8() {
	// A raw 0xff in the request line survives parsing. JSON is UTF-8, so a
	// byte outside any valid sequence becomes U+FFFD; valid characters stay.
	ctx := log_ctx(.get, '/a\xffb/\xc3\xa9/\xe2\x82', 200, '', 0, 'id\x80')
	line := format_access_log(AccessLogOptions{}, &ctx)
	assert line == '{"method":"GET","path":"/a\\ufffdb/\xc3\xa9/\\ufffd\\ufffd","status":200,"bytes":0,"latency_ms":-1,"request_id":"id\\ufffd"}'
	assert utf8.validate_str(line)
}

fn test_access_log_line_counts_the_bytes_of_a_file_response() {
	// A file response leaves the body empty and sends Content-Length only.
	mut ctx := log_ctx(.get, '/download', 200, '', 0, '')
	ctx.return_type = .file
	ctx.res.header.set(.content_length, '3000000000')
	line := format_access_log(AccessLogOptions{}, &ctx)
	assert line.contains('"bytes":3000000000')
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
	assert ctx.access_log_start_ns > 0
}

fn test_access_log_logs_the_size_of_a_served_file() {
	path := os.join_path(os.vtmp_dir(), 'veb_access_log_${os.getpid()}.txt')
	os.write_file(path, 'x'.repeat(30)) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut sink := &LogSink{}
	log := AccessLog[LogContext]{
		write: sink.writer()
	}
	mut app := LogApp{
		file_path: path
	}
	app.use(log.stamp())
	app.use(log.finish())
	ctx := render_request[LogApp, LogContext](mut app, http.Request{
		method: .get
		url:    '/download'
	}) or { panic(err) }
	assert ctx.return_type == .file
	assert sink.lines.len == 1
	assert sink.lines[0].contains('"path":"/download","status":200,"bytes":30,')
}

fn test_access_log_pair_built_in_a_helper_keeps_its_options() {
	// Setting the app up in a helper function is the usual shape; the halves
	// must not reach back into that helper's finished frame.
	mut sink := &LogSink{}
	mut app := LogApp{}
	use_access_log_with_query(mut app, sink)
	render_request[LogApp, LogContext](mut app, http.Request{
		method: .get
		url:    '/page?token=x'
	}) or { panic(err) }
	assert sink.lines.len == 1
	assert sink.lines[0].contains('"path":"/page?token=x"')
}

fn test_access_log_finish_keeps_the_options_it_was_built_with() {
	// The finish half copies what it needs when it is built: replacing the
	// value it came from afterwards must change nothing.
	mut sink := &LogSink{}
	mut other := &LogSink{}
	mut log := AccessLog[LogContext]{
		options: AccessLogOptions{
			include_query: true
		}
		write:   sink.writer()
	}
	mut app := LogApp{}
	app.use(log.stamp())
	app.use(log.finish())
	log = AccessLog[LogContext]{
		write: other.writer()
	}
	render_request[LogApp, LogContext](mut app, http.Request{
		method: .get
		url:    '/page?token=x'
	}) or { panic(err) }
	assert other.lines.len == 0
	assert sink.lines.len == 1
	assert sink.lines[0].contains('"path":"/page?token=x"')
}
