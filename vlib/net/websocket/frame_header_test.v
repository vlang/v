module websocket

import net
import time

fn frame_header_pair() !(&Client, &net.TcpConn) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut sender := net.dial_tcp(listener.addr()!.str())!
	mut receiver := listener.accept()!
	sender.set_write_timeout(2 * time.second)
	receiver.set_read_timeout(2 * time.second)
	client := &Client{
		is_server:    true
		conn:         receiver
		client_state: ClientState{
			state: .open
		}
	}
	return client, sender
}

fn test_parse_frame_header_rejects_top_bit_64bit_length() ! {
	mut client, mut sender := frame_header_pair()!
	defer { sender.close() or {} }
	defer { client.conn.close() or {} }
	// binary frame, unmasked, 64-bit length with the top bit set. RFC 6455
	// requires that bit to be 0; with it set, int(payload_len) goes negative
	// and read_payload allocated a negative sized array
	sender.write([u8(0x82), 0x7F, 0x80, 0, 0, 0, 0, 0, 0, 0])!
	if _ := client.parse_frame_header() {
		assert false, 'a 64-bit length with the top bit set must be rejected'
	} else {
		assert err.msg() == 'websocket: noncanonical 64-bit frame length'
	}
}

fn test_parse_frame_header_short_frame_still_parses() ! {
	mut client, mut sender := frame_header_pair()!
	defer { sender.close() or {} }
	defer { client.conn.close() or {} }
	// a normal short binary frame, 'hi', must keep parsing
	sender.write([u8(0x82), 0x02, `h`, `i`])!
	frame := client.parse_frame_header()!
	assert frame.opcode == .binary_frame
	assert frame.payload_len == 2
	assert frame.has_mask == false
}

fn frame_length_header(length u64, masked bool) []u8 {
	mut wire := [u8(0x82), if masked { u8(0xff) } else { u8(0x7f) }]
	for shift := 56; shift >= 0; shift -= 8 {
		wire << u8(length >> shift)
	}
	if masked { wire << [u8(1), 2, 3, 4] }
	return wire
}

fn test_parse_frame_header_rejects_length_above_int_range() ! {
	// With 64-bit ints the top-bit validation already covers this boundary.
	if u64(max_int) == u64(0x7fff_ffff_ffff_ffff) { return }
	for masked in [false, true] {
		mut client, mut sender := frame_header_pair()!
		defer { sender.close() or {} }
		defer { client.conn.close() or {} }
		wire := frame_length_header(u64(max_int) + 1, masked)
		assert sender.write(wire)! == wire.len
		if _ := client.parse_frame_header() {
			assert false, 'a payload larger than max_int must be rejected before allocation'
		} else {
			assert err.msg() == 'websocket: 64-bit frame length exceeds int range'
		}
	}
}

fn test_parse_frame_header_valid_64bit_length_and_int_boundary() ! {
	for masked in [false, true] {
		for length in [u64(65536), u64(max_int)] {
			mut client, mut sender := frame_header_pair()!
			defer { sender.close() or {} }
			defer { client.conn.close() or {} }
			wire := frame_length_header(length, masked)
			assert sender.write(wire)! == wire.len
			frame := client.parse_frame_header()!
			assert frame.opcode == .binary_frame
			assert frame.payload_len == int(length)
			assert frame.has_mask == masked
			if masked {
				assert frame.masking_key == [u8(1), 2, 3, 4]!
			}
		}
	}
}
