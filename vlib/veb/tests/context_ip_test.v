// vtest build: !windows // fasthttp.Server.run is not implemented on windows yet
import net.http
import time
import veb

const port = 13066
const localserver = 'http://127.0.0.1:${port}'
const exit_after = time.second * 10

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('ip=${ctx.ip()} conn_nil=${ctx.conn == unsafe { nil }}')
}

fn testsuite_begin() {
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()

	mut app := &App{}
	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2, family: .ip)
	_ := <-app.started
}

fn test_context_ip_uses_peer_ip_for_buffered_requests() {
	res := http.get(localserver)!
	assert res.body == 'ip=127.0.0.1 conn_nil=true'
}

fn test_context_ip_prefers_proxy_headers() {
	res := http.fetch(
		url:    localserver
		header: http.new_header_from_map({
			.x_forwarded_for: '9.9.9.9, 8.8.8.8'
		})
	)!
	assert res.body == 'ip=9.9.9.9 conn_nil=true'
}
