import x.vweb
import net.http
import os
import time

const port = 13001

const localserver = 'http://127.0.0.1:${port}'

const exit_after = time.second * 10

pub struct Context {
	vweb.Context
pub mut:
	counter int
}

@[heap]
pub struct App {
	vweb.Middleware[Context]
}

pub fn (app &App) index(mut ctx Context) vweb.Result {
	return ctx.text('from index, ${ctx.counter}')
}

@['/bar/bar']
pub fn (app &App) bar(mut ctx Context) vweb.Result {
	return ctx.text('from bar, ${ctx.counter}')
}

pub fn (app &App) unreachable(mut ctx Context) vweb.Result {
	return ctx.text('should never be reachable!')
}

@['/nested/route/method']
pub fn (app &App) nested(mut ctx Context) vweb.Result {
	return ctx.text('from nested, ${ctx.counter}')
}

pub fn (app &App) after(mut ctx Context) vweb.Result {
	return ctx.text('from after, ${ctx.counter}')
}

pub fn (app &App) app_middleware(mut ctx Context) bool {
	ctx.counter++
	return true
}

fn middleware_handler(mut ctx Context) bool {
	ctx.counter++
	return true
}

fn middleware_unreachable(mut ctx Context) bool {
	ctx.text('unreachable, ${ctx.counter}')
	return false
}

fn after_middleware(mut ctx Context) bool {
	ctx.counter++
	ctx.res.header.add_custom('X-AFTER', ctx.counter.str()) or { panic('bad') }
	return true
}

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!

	spawn fn () {
		mut app := &App{}
		// even though `route_use` is called first, global middleware is still executed first
		app.Middleware.route_use('/unreachable', handler: middleware_unreachable)

		// global middleware
		app.Middleware.use(handler: middleware_handler)
		app.Middleware.use(handler: app.app_middleware)

		// should match only one slash
		app.Middleware.route_use('/bar/:foo', handler: middleware_handler)
		// should match multiple slashes
		app.Middleware.route_use('/nested/:path...', handler: middleware_handler)

		app.Middleware.route_use('/after', handler: after_middleware, after: true)

		vweb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2, family: .ip) or {
			panic('could not start vweb app')
		}
	}()
	// app startup time
	time.sleep(time.second * 2)

	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn test_index() {
	x := http.get(localserver)!
	assert x.body == 'from index, 2'
}

fn test_unreachable_order() {
	x := http.get('${localserver}/unreachable')!
	assert x.body == 'unreachable, 2'
}

fn test_dynamic_route() {
	x := http.get('${localserver}/bar/bar')!
	assert x.body == 'from bar, 3'
}

fn test_nested() {
	x := http.get('${localserver}/nested/route/method')!
	assert x.body == 'from nested, 3'
}

fn test_after_middleware() {
	x := http.get('${localserver}/after')!
	assert x.body == 'from after, 2'

	custom_header := x.header.get_custom('X-AFTER') or { panic('should be set!') }
	assert custom_header == '3'
}

// TODO: add test for encode and decode gzip
