module main

import net.http
import veb

struct App {
mut:
	active int
	begins int
	ends   int
}

fn (mut app App) before_handle_request(_req http.Request) {
	app.active++
	app.begins++
}

fn (mut app App) after_handle_request(_req http.Request) {
	app.active--
	app.ends++
}

struct EmptyApp {}

struct Context {
	veb.Context
mut:
	message string
}

fn (app &App) index(mut ctx Context) veb.Result {
	assert app.active == 1
	ctx.message = 'external context'
	return ctx.text(ctx.message)
}

fn test_render_request_with_an_external_context_named_context() {
	mut app := App{}
	ctx := veb.render_request[App, Context](mut app, http.Request{ url: '/' })!
	assert ctx.res.body == 'external context'
	assert ctx.res.status_code == int(http.Status.ok)
	assert app.active == 0
	assert app.begins == 1 && app.ends == 1
}

struct PartialActivityApp {
mut:
	begins int
}

fn (mut app PartialActivityApp) before_handle_request(_req http.Request) {
	app.begins++
}

fn (app &PartialActivityApp) index(mut ctx Context) veb.Result {
	return ctx.text('partial hooks')
}

fn test_an_application_with_only_one_hook_keeps_existing_dispatch_behavior() {
	mut app := PartialActivityApp{}
	ctx := veb.render_request[PartialActivityApp, Context](mut app, http.Request{ url: '/' })!
	assert ctx.res.body == 'partial hooks'
	assert app.begins == 0
}

fn test_render_request_rejects_the_base_context_as_the_application_context() {
	mut app := EmptyApp{}
	veb.render_request[EmptyApp, veb.Context](mut app, http.Request{ url: '/' }) or {
		assert err.msg().contains('must embed veb.Context')
		return
	}
	assert false, 'the application must use a context that embeds veb.Context'
}
