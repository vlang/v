// panic_recovery_test.v - a panicking handler must not take the process down.
//
// `handle_route` runs every request handler, and before this change nothing
// between it and the accept loop recovered a panic: one bad index, one failed
// unwrap written with `!`, or one nil map access in any handler ended the
// server. `render_request` is used here rather than a listener because the
// outcome is identical on both paths - the panic unwinds into the same defer -
// and because the second test, "the server still serves afterwards", needs two
// sequential requests through one process.
module veb

import net.http

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
	assert ctx.res.status_code == int(http.Status.internal_server_error)
	assert ctx.res.body != ''
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
