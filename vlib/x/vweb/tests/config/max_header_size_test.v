import net.http
import time
import x.vweb

const port = 13021
const exit_after = time.second * 10
const header_size = 1000
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
		timeout_in_seconds: 2
		config: vweb.Config{
			max_header_len: header_size
		}
	)
	_ := <-app.started

	spawn fn () {
		time.sleep(exit_after)
		assert false, 'timeout reached'
		exit(1)
	}()
}

fn test_larger_header_size() {
	mut buf := []u8{len: header_size * 2, init: `a`}
	str := buf.bytestr()

	mut header := http.new_custom_header_from_map({
		'X-Large-Header': str
	})!

	mut x := http.fetch(http.FetchConfig{
		url: localserver
		header: header
	})!

	assert x.status() == .request_entity_too_large
}
