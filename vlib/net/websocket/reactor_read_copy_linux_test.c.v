module websocket

import net
import time

fn read_copy_echo(mut client ReactorClient, message &Message, _ref voidptr) {
	client.write(message.payload, message.opcode) or { panic(err) }
}

fn read_copy_wire(payload []u8) []u8 {
	mut wire := [u8(0x82)]
	if payload.len < 126 {
		wire << u8(0x80 | payload.len)
	} else if payload.len <= 65535 {
		wire << [u8(0xfe), u8(payload.len >> 8), u8(payload.len)]
	} else {
		wire << u8(0xff)
		for shift := 56; shift >= 0; shift -= 8 {
			wire << u8(u64(payload.len) >> shift)
		}
	}
	mask := [u8(3), 7, 11, 13]!
	for byte in mask { wire << byte }
	for i, byte in payload { wire << byte ^ mask[i % 4] }
	return wire
}

fn read_copy_write_all(mut tcp net.TcpConn, wire []u8, chunk int) ! {
	mut at := 0
	for at < wire.len {
		end := if at + chunk < wire.len { at + chunk } else { wire.len }
		n := tcp.write(wire[at..end])!
		assert n > 0
		at += n
	}
}

fn test_reactor_receive_copy_preserves_bytes_across_buffer_reuse() ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	mut tcp := net.dial_tcp(listener.addr()!.str())!
	mut accepted := listener.accept()!
	listener.close()!
	tcp.set_read_timeout(3 * time.second)
	mut reactor := new_reactor(
		max_message_bytes: 65536
		frames_per_turn:   3
		on_message:        read_copy_echo
	)!
	mut worker := spawn reactor.run()
	reactor.attach(mut accepted, '')!
	mut receiver := &Client{ conn: tcp, client_state: ClientState{ state: .open } }
	for turn in 0 .. 2 {
		for size in [0, 1, 125, 126, 4096, 16375, 16376, 16383, 16384, 16385, 32768, 65536] {
			payload := []u8{len: size, init: u8(index * 31 + turn)}
			wire := read_copy_wire(payload)
			read_copy_write_all(mut tcp, wire, if turn == 0 { 65536 } else { 997 })!
			message := receiver.read_next_message()!
			assert message.opcode == .binary_frame, 'turn ${turn}, size ${size}'
			assert message.payload == payload
		}
	}
	// Several complete frames share a read and exceed the per-turn frame budget.
	mut combined := []u8{}
	for i in 0 .. 37 { combined << read_copy_wire([u8(i), 0, 255]) }
	read_copy_write_all(mut tcp, combined, combined.len)!
	for i in 0 .. 37 {
		assert receiver.read_next_message()!.payload == [u8(i), 0, 255]
	}
	tcp.close()!
	reactor.stop()
	worker.wait()!
}
