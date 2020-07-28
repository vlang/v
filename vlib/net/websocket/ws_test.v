import websocket
import time

struct Test {
mut:
	connected    bool = false
	sent_messages     []string = []
	received_messages []string = []
}

fn test_ws() {
	ws_test('ws://echo.websocket.org')
	ws_test('wss://echo.websocket.org')
}

fn ws_test(uri string) {
	mut test := Test{}
	println('connecting to $uri ...')
	mut ws := websocket.new(uri)
	ws.subscriber.subscribe_method('on_open', on_open, test)
	ws.subscriber.subscribe_method('on_message', on_message, test)
	ws.subscriber.subscribe('on_error', on_error)
	ws.subscriber.subscribe_method('on_close', on_close, test)
	ws.connect()
	go ws.listen()
	text := ['ws test', '{"vlang": "test0\n192"}']
	for msg in text {
		test.sent_messages << msg
		len := ws.write(msg.str, msg.len, .text_frame)
		assert msg.len <= len
		// sleep to give time to recieve response before send a new one
		time.sleep_ms(100)
	}
	// sleep to give time to recieve response before asserts
	time.sleep_ms(500)

	assert test.connected == true
	assert test.sent_messages.len == test.received_messages.len
	for x, msg in test.sent_messages {
		assert msg == test.received_messages[x]
	}
}

fn on_open(ws &websocket.Client, x voidptr, mut test Test) {
	println('websocket opened.')
	test.connected = true
}

fn on_message(ws &websocket.Client, msg &websocket.Message, mut test Test) {
	typ := msg.opcode
	if typ == .text_frame {
		println('Message: ${cstring_to_vstring(msg.payload)}')
		test.received_messages << cstring_to_vstring(msg.payload)
	} else {
		println('Binary message: $msg')
	}
}

fn on_close(ws &websocket.Client, x, y voidptr) {
	println('websocket closed.')
}

fn on_error(ws &websocket.Client, x, y voidptr) {
	println('we have an error.')
	assert false
}
