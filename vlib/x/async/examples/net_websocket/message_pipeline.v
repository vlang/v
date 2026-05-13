import context
import net.websocket
import x.async as xasync

fn describe_message(msg websocket.Message) !string {
	return match msg.opcode {
		.text_frame {
			'text:${msg.payload.bytestr()}'
		}
		.ping {
			'ping'
		}
		else {
			error('unsupported websocket message opcode')
		}
	}
}

fn main() {
	messages := [
		websocket.Message{
			opcode:  .text_frame
			payload: 'hello'.bytes()
		},
		websocket.Message{
			opcode: .ping
		},
	]
	processed := chan string{cap: messages.len}
	mut group := xasync.new_group(context.background())

	for msg in messages {
		group.go(fn [msg, processed] (mut ctx context.Context) ! {
			done := ctx.done()
			select {
				_ := <-done {
					return ctx.err()
				}
				else {}
			}
			processed <- describe_message(msg)!
		})!
	}

	group.wait()!
	for _ in 0 .. messages.len {
		println(<-processed)
	}
}
