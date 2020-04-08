module websocket

fn (ws &Client) send_message_event(msg Message) {
	ws.eb.publish('on_message', ws, msg)
	l.d('sending on_message event')
}

fn (ws &Client) send_error_event(err string) {
	ws.eb.publish('on_error', ws, err)
	l.d('sending on_error event')
}

fn (ws &Client) send_close_event() {
	ws.eb.publish('on_close', ws, voidptr(0))
	l.d('sending on_close event')
}

fn (ws &Client) send_open_event() {
	ws.eb.publish('on_open', ws, voidptr(0))
	l.d('sending on_open event')
}
