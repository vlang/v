module websocket

import net
import time

fn batch_pair(server bool) !(&Client, &Client) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut sender := net.dial_tcp(listener.addr()!.str())!
	mut receiver := listener.accept()!
	sender.set_write_timeout(2 * time.second)
	receiver.set_read_timeout(2 * time.second)
	return &Client{ is_server: server, conn: sender, client_state: ClientState{ state: .open } }, &Client{ is_server: !server, conn: receiver, client_state: ClientState{ state: .open } }
}

fn send_batch(mut client Client, messages []Message) !int {
	return client.write_messages(messages)
}

fn test_batch_preserves_messages_and_length_boundaries() ! {
	for server in [true, false] {
		mut sender, mut receiver := batch_pair(server)!
		mut messages := []Message{}
		for length in [0, 125, 126, 65535, 65536] {
			messages << Message{ opcode: .binary_frame, payload: []u8{len: length, init: u8(index % 251)} }
		}
		messages << Message{ opcode: .ping, payload: 'probe'.bytes() }
		mut writer := spawn send_batch(mut sender, messages)
		for expected in messages {
			actual := receiver.read_next_message()!
			assert actual.opcode == expected.opcode
			assert actual.payload == expected.payload
		}
		assert writer.wait()! > 131000
		assert sender.write_messages([])! == 0
		sender.conn.close()!
		receiver.conn.close()!
	}
}

fn test_concurrent_batches_never_interleave_frames() ! {
	mut sender, mut receiver := batch_pair(true)!
	mut writers := []thread{}
	for i in 0 .. 4 {
		messages := [Message{ opcode: .binary_frame, payload: []u8{len: 131072, init: u8(i)} }]
		writers << spawn assert_send_batch(mut sender, messages)
	}
	mut seen := []u8{}
	for _ in 0 .. 4 {
		message := receiver.read_next_message()!
		assert message.payload.len == 131072
		value := message.payload[0]
		assert message.payload.all(it == value)
		assert value !in seen
		seen << value
	}
	writers.wait()
	sender.conn.close()!
	receiver.conn.close()!
}

fn assert_send_batch(mut client Client, messages []Message) {
	assert client.write_messages(messages) or { panic(err) } == 131082
}
