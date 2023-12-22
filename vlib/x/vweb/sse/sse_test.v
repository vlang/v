import x.vweb
import x.vweb.sse
import time
import net.http

const port = 13008
const localserver = 'http://127.0.0.1:${port}'
const exit_after = time.second * 10

pub struct Context {
	vweb.Context
}

pub struct App {}

fn (app &App) sse(mut ctx Context) vweb.Result {
	ctx.takeover_conn()
	spawn handle_sse_conn(mut ctx)
	return vweb.no_result()
}

fn handle_sse_conn(mut ctx Context) {
	mut sse_conn := sse.start_connection(mut ctx.Context)

	for _ in 0 .. 3 {
		time.sleep(time.second)
		sse_conn.send_message(data: 'ping') or { break }
	}
	sse_conn.close()
}

fn testsuite_begin() {
	mut app := &App{}

	spawn vweb.run_at[App, Context](mut app, port: port, family: .ip)
	// app startup time
	time.sleep(time.second * 2)
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn test_sse() ! {
	mut x := http.get('${localserver}/sse')!

	connection := x.header.get(.connection) or {
		assert true == false, 'Header Connection should be set!'
		panic('missing header')
	}
	cache_control := x.header.get(.cache_control) or {
		assert true == false, 'Header Cache-Control should be set!'
		panic('missing header')
	}
	content_type := x.header.get(.content_type) or {
		assert true == false, 'Header Content-Type should be set!'
		panic('missing header')
	}
	assert connection == 'keep-alive'
	assert cache_control == 'no-cache'
	assert content_type == 'text/event-stream'

	eprintln(x.body)
	assert x.body == 'data: ping\n\ndata: ping\n\ndata: ping\n\nevent: close\ndata: Closing the connection\nretry: -1\n\n'
}
