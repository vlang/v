# Server Sent Events

`vweb.sse` wraps a live `vweb` request TCP connection.
Create the connection inside a route handler; `app.conn` is nil before a request is
being handled.

## Usage

```v ignore
import time
import vweb
import vweb.sse

struct App {
	vweb.Context
}

@['/events']
pub fn (mut app App) events() vweb.Result {
	mut stream := sse.new_connection(app.conn)
	stream.start() or {
		return app.server_error(500)
	}
	for i in 0 .. 3 {
		time.sleep(time.second)
		stream.send_message(sse.SSEMessage{
			id: '${i}'
			event: 'ping'
			data: 'hello'
		}) or { break }
	}
	return vweb.Result{}
}
```
