module websocket

import (
	eventbus
)

fn (ws &Client) send_message_event(msg Message){
	mut params := eventbus.Params{}
	mut typ := ""
	if msg.opcode == .text_frame {
		params.put_string("payload", string(byteptr(msg.payload)))
		typ = 'string'
	} else if msg.opcode == .binary_frame {
		params.put_custom("payload", "binary", msg.payload)
		typ = 'binary'
	}
	params.put_string("type", typ)
	params.put_int("len", msg.payload_len)
	ws.eb.publish("on_message", params, ws)
	l.d("sending on_message event")
}

fn (ws &Client) send_error_event(err string) {
	mut params := eventbus.Params{}
	params.put_string("error", err)
	ws.eb.publish("on_error", params, ws)
	l.d("sending on_error event")
}

fn (ws &Client) send_close_event() {
	params := eventbus.Params{}
	ws.eb.publish("on_close", params, ws)
	l.d("sending on_close event")
}

fn (ws &Client) send_open_event() {
	params := eventbus.Params{}
	ws.eb.publish("on_open", params, ws)
	l.d("sending on_open event")
}