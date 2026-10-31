// result_return_test.v - a handler may answer with `!veb.Result`, not only `veb.Result`.
//
// A handler that can fail used to have two bad choices: hand-roll
// `ctx.res.set_status(...); return ctx.text(...)` in every fallible path, or
// not compile at all. A `!veb.Result` handler answers by returning the value
// and declines by returning an error, which the dispatcher turns into a JSON
// envelope. When the handler already chose a status, it is kept, so 4xx stays
// in the handler's hands; otherwise the envelope is a 500.
module veb

import net.http

struct FallibleApp {}

struct FallibleContext {
	Context
}

@['/fine']
fn (app &FallibleApp) fine(mut ctx FallibleContext) !Result {
	return ctx.text('all good')
}

@['/broken']
fn (app &FallibleApp) broken(mut _ctx FallibleContext) !Result {
	return error('the disk is on fire')
}

@['/missing']
fn (app &FallibleApp) missing(mut ctx FallibleContext) !Result {
	ctx.res.set_status(.not_found)
	return error('no such thing')
}

@['/plain']
fn (app &FallibleApp) plain(mut ctx FallibleContext) Result {
	return ctx.text('still plain')
}

// fallible_request renders one path through the dispatcher.
fn fallible_request(url string) &Context {
	mut app := FallibleApp{}
	return render_request[FallibleApp, FallibleContext](mut app, http.Request{
		method: .get
		url:    url
	}) or { panic(err) }
}

fn test_a_returned_value_answers_200() {
	ctx := fallible_request('/fine')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'all good'
}

fn test_a_returned_error_answers_500_with_a_json_envelope() {
	ctx := fallible_request('/broken')
	assert ctx.res.status_code == int(http.Status.internal_server_error)
	assert ctx.res.body.contains('the disk is on fire')
}

fn test_a_status_the_handler_chose_is_kept() {
	// The handler set 404 before declining, so the envelope carries the
	// handler's status rather than a 500.
	ctx := fallible_request('/missing')
	assert ctx.res.status_code == int(http.Status.not_found)
	assert ctx.res.body.contains('no such thing')
}

fn test_a_plain_result_handler_is_unaffected() {
	ctx := fallible_request('/plain')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'still plain'
}
