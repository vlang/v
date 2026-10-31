module websocket

import net
import time

fn header_test_frame(payload []u8, masked bool, opcode u8) []u8 {
	mut wire := [opcode]
	mask_bit := if masked { u8(0x80) } else { u8(0) }
	if payload.len < 126 {
		wire << (u8(payload.len) | mask_bit)
	} else if payload.len <= 65535 {
		wire << [u8(126) | mask_bit, u8(payload.len >> 8), u8(payload.len)]
	} else {
		wire << (u8(127) | mask_bit)
		for shift := 56; shift >= 0; shift -= 8 {
			wire << u8(u64(payload.len) >> shift)
		}
	}
	key := [u8(0x37), 0xfa, 0x21, 0x80]
	if masked {
		wire << key
	}
	for i, byte in payload {
		wire << if masked { byte ^ key[i % 4] } else { byte }
	}
	return wire
}

fn header_test_pair(masked bool) !(&Client, &net.TcpConn) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut sender := net.dial_tcp(listener.addr()!.str())!
	mut receiver := listener.accept()!
	receiver.set_read_timeout(2 * time.second)
	sender.set_write_timeout(2 * time.second)
	client := &Client{
		is_server:    masked
		conn:         receiver
		client_state: ClientState{
			state: .open
		}
	}
	return client, sender
}

fn header_test_write(mut sender net.TcpConn, frames [][]u8, split_headers bool) ! {
	defer { sender.close() or {} }
	if !split_headers {
		mut wire := []u8{}
		for frame in frames {
			wire << frame
		}
		mut offset := 0
		for offset < wire.len {
			written := sender.write(wire[offset..])!
			assert written > 0
			offset += written
		}
		return
	}
	for frame in frames {
		length_bytes := match frame[1] & 0x7f {
			126 { 2 }
			127 { 8 }
			else { 0 }
		}
		header_size := 2 + length_bytes + if frame[1] & 0x80 != 0 { 4 } else { 0 }
		for i in 0 .. header_size {
			assert sender.write(frame[i..i + 1])! == 1
			time.sleep(time.millisecond)
		}
		mut offset := header_size
		for offset < frame.len {
			end := if offset + 8192 < frame.len { offset + 8192 } else { frame.len }
			written := sender.write(frame[offset..end])!
			assert written > 0
			offset += written
		}
	}
}

fn test_header_reads_with_split_and_coalesced_frames() ! {
	lengths := [0, 1, 124, 125, 126, 127, 255, 256, 65535, 65536, 0, 1]
	for masked in [false, true] {
		mut expected := [][]u8{}
		mut frames := [][]u8{}
		for length in lengths {
			payload := []u8{len: length, init: u8((index * 31 + 7) % 251)}
			expected << payload
			frames << header_test_frame(payload, masked, 0x82)
		}
		for split_headers in [false, true] {
			mut client, mut sender := header_test_pair(masked)!
			mut writer := spawn header_test_write(mut sender, frames, split_headers)
			for payload in expected {
				message := client.read_next_message()!
				assert message.opcode == .binary_frame
				assert message.payload == payload
			}
			writer.wait()!
			client.conn.close()!
		}
	}
}

fn test_header_reads_with_fragmented_text_and_empty_control_frames() ! {
	payload := 'hello 世界 🌍'.bytes()
	for masked in [false, true] {
		// Split inside a UTF-8 code point, with an empty ping and continuation in between.
		frames := [header_test_frame(payload[..7], masked, 0x01),
			header_test_frame([]u8{}, masked, 0x89), header_test_frame([]u8{}, masked, 0x00),
			header_test_frame(payload[7..], masked, 0x80), header_test_frame([u8(42)], masked, 0x82)]
		mut client, mut sender := header_test_pair(masked)!
		mut writer := spawn header_test_write(mut sender, frames, false)
		ping := client.read_next_message()!
		assert ping.opcode == .ping
		assert ping.payload.len == 0
		message := client.read_next_message()!
		assert message.opcode == .text_frame
		assert message.payload == payload
		next := client.read_next_message()!
		assert next.opcode == .binary_frame
		assert next.payload == [u8(42)]
		writer.wait()!
		client.conn.close()!
	}
}

fn test_header_reads_propagate_eof_and_timeouts_in_each_field() ! {
	// Partial base header, 16/64-bit length, and each form of masking key.
	for partial in [[u8(0x82)], [u8(0x82), 126, 0], [u8(0x82), 127, 0, 0, 0], [u8(0x82), 0x80,
		1, 2], [u8(0x82), 0xfe, 0, 126, 1, 2], [u8(0x82), 0xff, 0, 0, 0, 0, 0, 1, 0, 0, 1, 2]] {
		for close_sender in [false, true] {
			mut client, mut sender := header_test_pair(partial.len > 1 && partial[1] & 0x80 != 0)!
			client.conn.set_read_timeout(100 * time.millisecond)
			assert sender.write(partial)! == partial.len
			if close_sender {
				sender.close()!
			}
			mut failed := false
			mut error_code := 0
			client.parse_frame_header() or {
				error_code = err.code()
				failed = true
			}
			client.conn.close()!
			if !close_sender {
				sender.close()!
			}
			assert failed, 'Partial header must propagate EOF or timeout'
			if !close_sender {
				assert error_code == net.err_timed_out_code
			}
		}
	}
}

fn test_buffered_reader_retains_coalesced_frames_and_zero_length_reads() ! {
	mut client, mut sender := header_test_pair(true)!
	defer { client.conn.close() or {} }
	defer { sender.close() or {} }
	first := header_test_frame('one'.bytes(), true, 0x81)
	second := header_test_frame('two'.bytes(), true, 0x81)
	mut wire := first.clone()
	wire << second
	assert sender.write(wire)! == wire.len
	assert client.read_next_message()!.payload.bytestr() == 'one'
	// No syscall or buffer consumption for an empty request.
	before := client.read_start
	mut byte := u8(0)
	assert client.socket_read_ptr(&byte, 0)! == 0
	assert client.read_start == before
	assert client.read_next_message()!.payload.bytestr() == 'two'
}
