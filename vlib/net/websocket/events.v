module websocket

fn (mut ws Client) send_message_event(msg Message) {
	ws.log.debug('sending on_message event')
	ws.eb.publish('on_message', ws, msg)
}

fn (mut ws Client) send_error_event(err string) {
	ws.log.debug('sending on_error event')
	ws.eb.publish('on_error', ws, err)
}

fn (mut ws Client) send_close_event() {
	ws.log.debug('sending on_close event')
	ws.eb.publish('on_close', ws, voidptr(0))
}

fn (mut ws Client) send_open_event() {
	ws.log.debug('sending on_open event')
	ws.eb.publish('on_open', ws, voidptr(0))
}
