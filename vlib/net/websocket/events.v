module websocket

fn (mut ws Client) send_message_event(msg &Message) {
	ws.eb.publish('on_message', ws, msg)
	ws.log.debug('sending on_message event')
}

fn (mut ws Client) send_error_event(err string) {
	ws.eb.publish('on_error', ws, err)
	ws.log.debug('sending on_error event')
}

fn (mut ws Client) send_close_event() {
	ws.eb.publish('on_close', ws, voidptr(0))
	ws.log.debug('sending on_close event')
}

fn (mut ws Client) send_open_event() {
	ws.eb.publish('on_open', ws, voidptr(0))
	ws.log.debug('sending on_open event')
}
