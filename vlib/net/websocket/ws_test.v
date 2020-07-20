import net.websocket
import sync

struct Test {
	last_send	string
	index		int
	mut:
		mtx		&sync.Mutex = sync.new_mutex()
		
}

fn test_ws() {
	ws := websocket.new('ws://echo.websocket.org')
	ws_test(ws)
}

fn test_wss() {
	wss := websocket.new('wss://echo.websocket.org')
	ws_test(wss)
}

fn ws_test(mut ws websocket.Client) {
	ws.subscriber.subscribe('on_open', on_open)
	ws.subscriber.subscribe('on_message', on_message)
	ws.subscriber.subscribe('on_error', on_error)
	ws.subscriber.subscribe('on_close', on_close)

	ws.connect()
	go ws.listen()

	mut wg := sync.new_waitgroup()
	wg.add(1)
	for i := 0; i < 10; i++ {
		text := 'a$i'
		ws.write(text.str, text.len, .text_frame)
		wg.done()
	}
	wg.wait()
}

fn on_open(sender voidptr, ws &websocket.Client, x voidptr) {
	println('websocket opened.')
}

fn on_message(sender voidptr, mut ws websocket.Client, msg &websocket.Message) {
	println('Message recieved. Sending it back.')
	typ := msg.opcode
	if typ == .text_frame {
		println("Message: $msg")
	} else {
		println('Binary message.')
	}
}

fn on_close(sender voidptr, ws &websocket.Client, x voidptr) {
	println('websocket closed.')
}

fn on_error(sender voidptr, ws &websocket.Client, x voidptr) {
	println('we have an error.')
}