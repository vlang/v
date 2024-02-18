import time
import x.vweb
import net
import net.http

const port = 13022
const exit_after = time.second * 10
const body_len = 1000
const localserver = 'http://localhost:${port}'

pub struct Context {
	vweb.Context
}

pub struct App {
mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (mut app App) index(mut ctx Context) vweb.Result {
	return ctx.text('Hello V!')
}

fn testsuite_begin() {
	mut app := &App{}
	spawn vweb.run_at[App, Context](mut app,
		port: port
		timeout_in_seconds: 15
		config: vweb.Config{
			max_request_body_len: body_len
		}
	)
	_ := <-app.started

	spawn fn () {
		time.sleep(exit_after)
		assert false, 'timeout reached'
		exit(1)
	}()
}

fn test_larger_body_len() {
	mut buf := []u8{len: body_len * 2, init: `a`}
	str := buf.bytestr()

	mut x := http.fetch(http.FetchConfig{
		url: localserver
		data: str
	})!

	assert x.status() == .request_entity_too_large
}
