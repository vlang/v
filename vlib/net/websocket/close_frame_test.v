module websocket

import net
import time

fn close_frame_pair(server bool) !(&Client, &Client) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut sender := net.dial_tcp(listener.addr()!.str())!
	mut receiver := listener.accept()!
	sender.set_write_timeout(2 * time.second)
	receiver.set_read_timeout(2 * time.second)
	return &Client{
		is_server:    server
		conn:         sender
		client_state: ClientState{
			state: .open
		}
	}, &Client{
		is_server:    !server
		conn:         receiver
		client_state: ClientState{
			state: .open
		}
	}
}

fn test_close_frame_rejects_oversized_reason() ! {
	mut sender, mut receiver := close_frame_pair(false)!
	// a close reason longer than 123 bytes would overflow the 7 bit length byte
	if _ := sender.close(1000, 'a'.repeat(200)) {
		assert false, 'an oversized close reason must be rejected'
	} else {
		assert err.msg() == 'control frame payload exceeds 125 bytes'
	}
	sender.conn.close() or {}
	receiver.conn.close() or {}
}

fn test_close_frame_sends_a_short_reason_intact() ! {
	mut sender, mut receiver := close_frame_pair(false)!
	reason := 'bye'
	sender.close(1000, reason)!
	msg := receiver.read_next_message()!
	assert msg.opcode == .close
	// the payload is the 2 byte status code followed by the reason
	assert msg.payload.len == 2 + reason.len
	assert msg.payload[2..].bytestr() == reason
	sender.conn.close() or {}
	receiver.conn.close() or {}
}
