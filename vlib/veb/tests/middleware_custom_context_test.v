// vtest build: !windows // fasthttp.Server.run is not implemented on windows yet
// An app method registered with the promoted `app.use` must be called with the app's own
// `Context`, not with the embedded `veb.Context` of the same name.
// See https://github.com/vlang/v/issues/28812
import veb
import net.http
import time

const port = 13047

const localserver = 'http://127.0.0.1:${port}'

const exit_after = time.second * 10

pub struct Context {
	veb.Context
pub mut:
	seen string
}

@[heap]
pub struct App {
	veb.Middleware[Context]
mut:
	started chan bool
	hits    int
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

fn (mut app App) middleware_debug(mut ctx Context) bool {
	app.hits++
	ctx.seen = '${ctx.req.method} ${ctx.req.url}'
	return true
}

pub fn (app &App) index(mut ctx Context) veb.Result {
	return ctx.text('${ctx.seen}, ${app.hits}')
}

fn testsuite_begin() {
	mut app := &App{}
	app.use(handler: app.middleware_debug)
	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2, family: .ip)
	_ := <-app.started
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn test_app_method_middleware_receives_custom_context() {
	x := http.get(localserver)!
	assert x.body == 'GET /, 1'
	y := http.get('${localserver}/?page=2')!
	assert y.body == 'GET /?page=2, 2'
}
