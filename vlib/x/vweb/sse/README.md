# Server Sent Events

This module implements the server side of `Server Sent Events`, SSE.
See [mozilla SSE][mozilla_sse]
as well as [whatwg][whatwg html spec]
for detailed description of the protocol, and a simple web browser client example.

## Usage

With SSE we want to keep the connection open, so we are able to 
keep sending events to the client. But if we hold the connection open indefinitely
vweb isn't able to process any other requests. 

We can let vweb know that it can continue processing other requests and that we will 
handle the connection ourself by calling `ctx.takeover_conn()` and and returning an empty result
with `vweb.no_result()`. Vweb will not close the connection and we can handle
the connection in a seperate thread.

**Example:**
```v ignore
import x.vweb.sse

// endpoint handler for SSE connections
fn (app &App) sse(mut ctx Context) vweb.Result {
	// let vweb know that the connection should not be closed
	ctx.takeover_conn()
	// handle the connection in a new thread
	spawn handle_sse_conn(mut ctx)
	// we will send a custom response ourself, so we can safely return an empty result
	return vweb.no_result()
}

fn handle_sse_conn(mut ctx Context) {
	// pass vweb.Context
	mut sse_conn := sse.start_connection(mut ctx.Context)

	// send a message every second 3 times
	for _ in 0.. 3 {
		time.sleep(time.second)
		sse_conn.send_message(data: 'ping') or { break }
	}
	// close the SSE connection
	sse_conn.close()
}
```

Javascript code:
```js
const eventSource = new EventSource('/sse');

eventSource.addEventListener('message', (event) => {
	console.log('received mesage:', event.data);
});

eventSource.addEventListener('close', () => {
	console.log('closing the connection')
	// prevent browser from reconnecting
	eventSource.close();
});
```

[mozilla_sse]: https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events
[whatwg html spec]: https://html.spec.whatwg.org/multipage/server-sent-events.html#server-sent-events