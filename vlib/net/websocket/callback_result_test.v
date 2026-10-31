// vtest build: !windows
import net.websocket
import time

@[heap]
struct PushCallbackContext {
	messages chan string
}

fn record_push_message(mut context &PushCallbackContext, msg &websocket.Message) {
	if msg.opcode == .text_frame {
		context.messages <- msg.payload.bytestr()
	}
}

fn callback_pusher(mut sc websocket.ServerClient) {
	time.sleep(200 * time.millisecond)
	for i in 0 .. 5 {
		sc.client.write_string('event ${i}') or { return }
	}
}

fn test_push_and_reply_with_void_and_reference_callbacks() ! {
	mut server := websocket.new_server(.ip, 30019, '')
	server.on_attached(fn (mut sc websocket.ServerClient) {
		spawn callback_pusher(mut sc)
	})
	server.on_message(fn (mut ws websocket.Client, msg &websocket.Message) {
		ws.write_string(msg.payload.bytestr()) or { panic(err) }
	})
	spawn server.listen()
	started := time.now()
	for server.get_state() != .open {
		assert time.since(started) < 3 * time.second, 'websocket server did not start'
		time.sleep(10 * time.millisecond)
	}
	context := &PushCallbackContext{ messages: chan string{cap: 6} }
	mut client := websocket.new_client('ws://127.0.0.1:30019')!
	client.on_open(fn (mut ws websocket.Client) {})
	client.on_error(fn (mut ws websocket.Client, err string) { panic(err) })
	client.on_message_ref(fn (mut ws websocket.Client, msg &websocket.Message, ctx voidptr) {
		// The callback API keeps the original reference, cast to its concrete type.
		record_push_message(mut unsafe { &PushCallbackContext(ctx) }, msg)
	}, context)
	client.connect()!
	defer { client.close(1000, 'done') or {} }
	spawn client.listen()
	for i in 0 .. 5 {
		select {
			message := <-context.messages {
				assert message == 'event ${i}'
			}
			3 * time.second {
				assert false, 'timed out waiting for server push ${i}'
			}
		}
	}
	client.write_string('reply')!
	select {
		message := <-context.messages {
			assert message == 'reply'
		}
		3 * time.second {
			assert false, 'timed out waiting for callback reply'
		}
	}
}
