module main

import veb
import rand

const port = 8082

struct State {
mut:
	cnt int
}

pub struct App {
mut:
	state shared State
}

struct Context {
	veb.Context
}

pub fn (app &App) before_request() {
	$if trace_before_request ? {
		eprintln('[veb] before_request: ${app.req.method} ${app.req.url}')
	}
}

@['/users/:user']
pub fn (mut app App) user_endpoint(mut ctx Context, user string) veb.Result {
	// pub fn (mut app App) user_endpoint(user string) veb.Result {
	id := rand.intn(100) or { 0 }
	return ctx.json({
		user: id
	})
}

pub fn (mut app App) index() veb.Result {
	mut c := 0
	lock app.state {
		app.state.cnt++
		c = app.state.cnt
		//
		$if trace_address_of_app_state_cnt ? {
			dump(ptr_str(app.state.cnt))
		}
	}
	show := true
	hello := 'Hello world from veb, request number: ${c}'
	numbers := [1, 2, 3]
	return $veb.html()
}

pub fn (mut app App) custom_template(mut ctx Context) veb.Result {
	return $veb.html('custom.html')
}

pub fn (mut app App) show_text(mut ctx Context) veb.Result {
	return ctx.text('Hello world from veb')
}

pub fn (mut app App) cookie(mut ctx Context) veb.Result {
	ctx.set_cookie(name: 'cookie', value: 'test')
	return ctx.text('Response Headers\n${ctx.req.header}')
}

@[post]
pub fn (mut app App) post(mut ctx Context) veb.Result {
	return ctx.text('Post body: ${ctx.req.data}')
}

fn main() {
	println('veb example')
	// veb.run(&App{}, port)
	mut app := &App{}
	veb.run_at[App, Context](mut app, port: port, family: .ip, timeout_in_seconds: 2) or {
		panic(err)
	}
}
