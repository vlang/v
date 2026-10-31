module websocket

import net
import time

fn ownership_write(mut client Client, values [][]u8) {
	for value in values {
		client.write(value, .binary_frame) or { panic(err) }
	}
}

fn test_received_payloads_remain_owned_after_later_reads() ! {
	for server in [false, true] {
		mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
		mut tcp := net.dial_tcp(listener.addr()!.str())!
		mut accepted := listener.accept()!
		listener.close()!
		tcp.set_write_timeout(2 * time.second)
		accepted.set_read_timeout(2 * time.second)
		mut sender := &Client{ conn: tcp, is_server: server, client_state: ClientState{ state: .open } }
		mut receiver := &Client{ conn: accepted, is_server: !server, client_state: ClientState{ state: .open } }
		mut values := [][]u8{}
		for length in [0, 125, 126, 65535, 65536, 3] {
			values << []u8{len: length, init: u8(index % 253)}
		}
		mut writer := spawn ownership_write(mut sender, values)
		mut received := []Message{}
		for _ in values { received << receiver.read_next_message()! }
		writer.wait()
		for i, message in received {
			assert message.payload == values[i]
			unsafe { message.free() }
		}
		sender.conn.close()!
		receiver.conn.close()!
	}
}
