module main

import vweb
import vweb.sse
import net
import time
import rand

struct App {
	vweb.Context
mut:
	sse_connections shared []sse.SSEConnection
}

@['/sse'; get]
fn (mut app App) sse() ?vweb.Result {
	println('sse')
	mut conn := sse.new_connection(app.conn)

	conn.headers['Access-Control-Allow-Origin'] = '*'

	conn.start() or {
		println('err: ${err}')
		return app.server_error(501)
	}

	lock app.sse_connections {
		app.sse_connections << conn
	}

	// Remove `for {}`to have a timeout
	for {
		time.sleep(60 * time.second)
	}

	return none
}

// This not work (there are not connection)
@['/notification'; post]
fn (mut app App) notification() vweb.Result {
	lock app.sse_connections {
		for mut conn in app.sse_connections {
			conn.send_message(sse.SSEMessage{
				id: rand.uuid_v4()
				event: 'notification'
				data: app.req.data
				retry: 3000
			}) or { eprintln(err) }
		}
	}

	return app.text('Notification received')
}

fn main() {
	shared sse_connections := []sse.SSEConnection{}

	vweb.run(&App{
		sse_connections: sse_connections
	}, 3001)
}
