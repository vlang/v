import context
import net.websocket
import x.async as xasync

fn test_net_websocket_task_processes_message_in_memory() {
	msg := websocket.Message{
		opcode:  .text_frame
		payload: 'hello'.bytes()
	}
	mut task := xasync.run[string](fn [msg] (mut ctx context.Context) !string {
		_ = ctx
		return websocket_message_text(msg)!
	})!
	assert task.wait()! == 'hello'
}

fn test_net_websocket_callback_error_propagates_through_group() {
	mut group := xasync.new_group(context.background())
	observed := chan string{cap: 1}
	group.go(fn [observed] (mut ctx context.Context) ! {
		_ = ctx
		msg := websocket.Message{
			opcode: .close
		}
		text := websocket_message_text(msg) or {
			observed <- err.msg()
			return err
		}
		observed <- text
	})!
	group.wait() or {
		assert err.msg() == 'unsupported websocket opcode'
		assert <-observed == 'unsupported websocket opcode'
		return
	}
	assert false
}

fn websocket_message_text(msg websocket.Message) !string {
	if msg.opcode != .text_frame {
		return error('unsupported websocket opcode')
	}
	return msg.payload.bytestr()
}
