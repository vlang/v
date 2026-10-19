// panic_recovery_test.v - a panicking handler must not take the process down.
//
// `handle_route` runs every request handler, and before this change nothing
// between it and the accept loop recovered a panic: one bad index, one failed
// unwrap written with `!`, or one nil map access in any handler ended the
// server. `render_request` is used here rather than a listener because the
// outcome is identical on both paths - the panic unwinds into the same defer -
// and because the test that the server still serves afterwards needs two
// sequential requests through one process.
module veb

import fasthttp
import net.http
import os

struct PanickingApp {}

struct PanickingContext {
	Context
}

@['/boom']
fn (mut app PanickingApp) boom(mut ctx PanickingContext) Result {
	items := ['a', 'b']
	// An out-of-range read stands in for any handler bug that panics.
	return ctx.text(items[9])
}

@['/survives']
fn (mut app PanickingApp) survives(mut ctx PanickingContext) Result {
	return ctx.text('alive')
}

@['/prepared-text']
fn (mut app PanickingApp) prepared_text(mut ctx PanickingContext) Result {
	ctx.text('private partial response'.repeat(3))
	panic('failure after text')
}

@['/prepared-html']
fn (mut app PanickingApp) prepared_html(mut ctx PanickingContext) Result {
	ctx.set_content_type('application/partial')
	ctx.set_header(.content_length, '999')
	ctx.set_header(.content_encoding, 'gzip')
	ctx.set_header(.transfer_encoding, 'chunked')
	ctx.set_header(.trailer, 'X-Checksum')
	ctx.set_header(.content_range, 'bytes 0-998/999')
	ctx.set_header(.accept_ranges, 'bytes')
	ctx.set_header(.content_disposition, 'attachment; filename="partial.html"')
	ctx.set_header(.etag, '"partial"')
	ctx.set_header(.last_modified, 'Wed, 21 Oct 2015 07:28:00 GMT')
	ctx.set_header(.cache_control, 'public, max-age=3600')
	ctx.set_header(.expires, 'Wed, 21 Oct 2037 07:28:00 GMT')
	ctx.set_header(.age, '60')
	ctx.set_header(.location, '/partial')
	ctx.already_compressed = true
	ctx.set_custom_header('X-Request-ID', 'panic-test') or { panic(err) }
	ctx.set_header(.access_control_allow_origin, 'https://example.test')
	ctx.html('<html>private partial response</html>')
	panic('failure after html')
}

@['/prepared-headers']
fn (mut app PanickingApp) prepared_headers(mut ctx PanickingContext) Result {
	ctx.set_header(.content_type, 'application/json')
	ctx.set_header(.content_length, '999')
	panic('failure before body')
}

@['/prepared-file']
fn (mut app PanickingApp) prepared_file(mut ctx PanickingContext) Result {
	ctx.file(os.join_path(@VMODROOT, 'vlib', 'veb', 'tests', 'testdata', 'root.txt'))
	panic('failure after file')
}

@['/manual-takeover']
fn (mut app PanickingApp) manual_takeover(mut ctx PanickingContext) Result {
	ctx.takeover_conn()
	ctx.text('owned response')
	panic('failure after manual takeover')
}

@['/reusable-takeover']
fn (mut app PanickingApp) reusable_takeover(mut ctx PanickingContext) Result {
	ctx.takeover_conn_reusable()
	ctx.text('owned response')
	panic('failure after reusable takeover')
}

// panic_request renders one path through the same path a server request takes.
fn panic_request(path string) &Context {
	mut app := PanickingApp{}
	return render_request[PanickingApp, PanickingContext](mut app, http.Request{
		method: .get
		url:    path
	}) or { panic(err) }
}

fn test_panicking_handler_answers_500_instead_of_exiting() {
	ctx := panic_request('/boom')
	assert_panic_response(ctx)
}

fn assert_panic_response(ctx &Context) {
	assert ctx.res.status_code == int(http.Status.internal_server_error)
	assert ctx.res.body == 'Internal Server Error'
	assert ctx.res.header.get(.content_type) or { '' } == 'text/plain'
	assert ctx.res.header.get(.content_length) or { '' } == ctx.res.body.len.str()
	assert ctx.return_type == .normal
	assert ctx.return_file == ''
	assert !ctx.already_compressed
	raw := ctx.res.bytestr()
	assert raw.starts_with('HTTP/1.1 500 Internal Server Error\r\n')
	assert raw.ends_with('\r\n\r\nInternal Server Error')
}

fn test_panic_replaces_a_prepared_text_body() {
	assert_panic_response(panic_request('/prepared-text'))
}

fn test_panic_replaces_prepared_response_metadata_and_preserves_other_headers() {
	ctx := panic_request('/prepared-html')
	assert_panic_response(ctx)
	for header in [http.CommonHeader.content_encoding, .transfer_encoding, .trailer, .content_range,
		.content_disposition, .accept_ranges, .etag, .last_modified, .cache_control, .expires,
		.age, .location] {
		assert !ctx.res.header.contains(header), '${header} survived the panic'
	}
	assert ctx.res.header.get_custom('X-Request-ID') or { '' } == 'panic-test'
	assert ctx.res.header.get(.access_control_allow_origin) or { '' } == 'https://example.test'
}

fn test_panic_discards_headers_prepared_before_a_response_body() {
	assert_panic_response(panic_request('/prepared-headers'))
}

fn test_panic_discards_a_prepared_file_response() {
	assert_panic_response(panic_request('/prepared-file'))
}

fn test_panic_does_not_replace_a_taken_over_response() {
	for path in ['/manual-takeover', '/reusable-takeover'] {
		ctx := panic_request(path)
		assert ctx.takeover_mode != .none
		assert ctx.res.status_code == int(http.Status.ok)
		assert ctx.res.body == 'owned response'
		assert ctx.res.header.get(.content_length) or { '' } == '14'
	}
}

fn test_panic_response_writer_does_not_stream_the_prepared_file() {
	mut app := PanickingApp{}
	routes := generate_routes[PanickingApp, PanickingContext](app) or { panic(err) }
	params := RequestParams{
		global_app: unsafe { voidptr(&app) }
		routes:     &routes
	}
	for path in ['/survives', '/prepared-file'] {
		mut out := []u8{}
		mut control := fasthttp.ResponseControl{}
		step := parallel_append_handler[PanickingApp, PanickingContext](fasthttp.HttpRequest{
			buffer:         'GET ${path} HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()
			client_conn_fd: -1
			user_data:      unsafe { voidptr(&params) }
		}, mut out, unsafe { nil }, mut control)
		assert step == .done
		assert control.file_path == ''
		assert control.should_close
		raw := out.bytestr()
		if path == '/survives' {
			assert raw.starts_with('HTTP/1.1 200 OK\r\n')
			assert raw.ends_with('\r\n\r\nalive')
		} else {
			assert raw.starts_with('HTTP/1.1 500 Internal Server Error\r\n')
			assert raw.contains('Content-Length: 21\r\n')
			assert raw.ends_with('\r\n\r\nInternal Server Error')
		}
	}
}

fn test_the_render_path_still_serves_after_a_handler_panics() {
	// The load-bearing pair. Before the recovery defer, the first line below
	// ended the process and the second could never run.
	boom := panic_request('/boom')
	assert boom.res.status_code == int(http.Status.internal_server_error)

	ctx := panic_request('/survives')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'alive'
}
