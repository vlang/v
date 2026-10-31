module main

import net.http
import veb

struct App {}

struct EmptyApp {}

struct Context {
	veb.Context
mut:
	message string
}

fn (app &App) index(mut ctx Context) veb.Result {
	ctx.message = 'external context'
	return ctx.text(ctx.message)
}

fn test_render_request_with_an_external_context_named_context() {
	mut app := App{}
	ctx := veb.render_request[App, Context](mut app, http.Request{ url: '/' })!
	assert ctx.res.body == 'external context'
	assert ctx.res.status_code == int(http.Status.ok)
}

fn test_render_request_rejects_the_base_context_as_the_application_context() {
	mut app := EmptyApp{}
	veb.render_request[EmptyApp, veb.Context](mut app, http.Request{ url: '/' }) or {
		assert err.msg().contains('must embed veb.Context')
		return
	}
	assert false, 'the application must use a context that embeds veb.Context'
}
