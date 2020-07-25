module main

import net.websocket
import time

fn main() {
	//URLs working for testing, reply the same sent messages
	ws_test('ws://echo.websocket.org')
	ws_test('wss://echo.websocket.org')
}

fn ws_test(uri string) {
	println('connecting to $uri ...')
	mut ws := websocket.new(uri)
	ws.subscriber.subscribe('on_open', on_open)
	ws.subscriber.subscribe('on_message', on_message)
	ws.subscriber.subscribe('on_error', on_error)
	ws.subscriber.subscribe('on_close', on_close)
	ws.connect()
	// Needs another thread, generates an infinite loop for listen
	go ws.listen()
	for i := 0; i < 10; i++ {
		text := 'a'.repeat(i)
		println(text)
		// Send a text to the server
		ws.write(text.str, text.len, .text_frame)
		// Only for test purposes, to give time to receive message
		time.sleep_ms(100)
	}
	// Only for test purposes, to give time to receive message
	time.sleep_ms(100)
}

fn on_open(ws &websocket.Client, x, y voidptr) {
	println('websocket opened.')
}

fn on_message(ws &websocket.Client, msg &websocket.Message, x voidptr) {
	typ := msg.opcode
	if typ == .text_frame {
		println('Message: ${cstring_to_vstring(msg.payload)}')
	} else {
		println('Binary message: $msg')
	}
}

fn on_close(ws &websocket.Client, x, y voidptr) {
	println('websocket closed.')
}

fn on_error(ws &websocket.Client, x, y voidptr) {
	println('we have an error.')
}
