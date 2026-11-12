module websocket

import net
import io
import time

fn header_test_frame(payload []u8, masked bool, opcode u8) []u8 {
	mut wire := [opcode]
	unsafe { wire.flags.set(.noslices) }
	defer { unsafe { wire.flags.clear(.noslices) } }
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

fn header_test_declared_frame(payload_len int, masked bool, opcode u8) []u8 {
	mut wire := [opcode]
	unsafe { wire.flags.set(.noslices) }
	defer { unsafe { wire.flags.clear(.noslices) } }
	mask_bit := if masked { u8(0x80) } else { u8(0) }
	if payload_len < 126 {
		wire << (u8(payload_len) | mask_bit)
	} else if payload_len <= 65535 {
		wire << [u8(126) | mask_bit, u8(payload_len >> 8), u8(payload_len)]
	} else {
		wire << (u8(127) | mask_bit)
		for shift := 56; shift >= 0; shift -= 8 {
			wire << u8(u64(payload_len) >> shift)
		}
	}
	if masked {
		wire << [u8(0x37), 0xfa, 0x21, 0x80]
	}
	return wire
}

fn header_test_assert_close_code(mut sender net.TcpConn, is_server bool, expected_code int) ! {
	wire := io.read_all(reader: sender)!
	assert wire.len >= 4
	assert wire[0] == 0x88
	close_is_masked := wire[1] & 0x80 != 0
	assert close_is_masked == !is_server
	header_len := if close_is_masked { 6 } else { 2 }
	payload_len := int(wire[1] & 0x7f)
	assert payload_len >= 2
	assert wire.len == header_len + payload_len
	mut status := [u8(0), 0]
	for i in 0 .. 2 {
		status[i] = wire[header_len + i]
		if close_is_masked {
			status[i] ^= wire[2 + i % 4]
		}
	}
	assert (int(status[0]) << 8 | int(status[1])) == expected_code
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
		mut wire_len := 0
		for frame in frames {
			wire_len += frame.len
		}
		mut wire := []u8{cap: wire_len}
		defer { unsafe { wire.free() } }
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

fn test_client_and_server_options_set_message_limit() ! {
	mut client := new_client('ws://127.0.0.1/', ClientOpt{ max_message_bytes: 42 })!
	assert client.max_message_bytes == 42
	client.free()

	mut server := new_server(.ip, 0, '/', ServerOpt{ max_message_bytes: 24 })
	assert server.max_message_bytes == 24
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	server.ls = listener
	mut peer := net.dial_tcp(listener.addr()!.str())!
	mut accepted := server.accept_new_client()!
	assert accepted.max_message_bytes == 24
	accepted.conn.close()!
	peer.close()!
	listener.close()!
}

fn test_reader_rejects_declared_oversize_before_payload_and_sends_1009() ! {
	for is_server in [false, true] {
		mut client, mut sender := header_test_pair(is_server)!
		header := header_test_declared_frame(default_max_message_bytes + 1, is_server, 0x82)
		assert sender.write(header)! == header.len
		if _ := client.read_next_message() {
			assert false, 'oversized message must be rejected from its header'
		} else {
			assert err.msg().contains('configured size limit')
		}
		header_test_assert_close_code(mut sender, is_server, 1009)!
		sender.close()!
	}
}

fn test_reader_releases_payload_buffer_when_peer_closes_mid_frame() ! {
	for _ in 0 .. 8 {
		mut client, mut sender := header_test_pair(true)!
		client.max_message_bytes = 1024 * 1024
		header := header_test_declared_frame(client.max_message_bytes, true, 0x82)
		mut wire := header.clone()
		wire << u8(0)
		assert sender.write(wire)! == wire.len
		sender.close()!
		mut failed := false
		if _ := client.read_next_message() {
			assert false, 'incomplete payload must fail'
		} else {
			failed = true
		}
		assert failed, 'incomplete payload must fail after a partial read'
		client.conn.close()!
	}
}

fn test_reader_releases_fragment_buffers_after_invalid_text() ! {
	mut client, mut sender := header_test_pair(true)!
	client.max_message_bytes = 5
	first := header_test_frame([u8(0xc3)], true, 0x01)
	last := header_test_frame([u8(`(`)], true, 0x80)
	assert sender.write(first)! == first.len
	assert sender.write(last)! == last.len
	if _ := client.read_next_message() {
		assert false, 'invalid fragmented text must fail UTF-8 validation'
	} else {
		assert err.msg().contains('malformed utf8 payload')
	}
	header_test_assert_close_code(mut sender, true, 1007)!
	sender.close()!
}

fn test_reader_rejects_invalid_fragment_sequences() ! {
	for is_server in [false, true] {
		for invalid_sequence in [0, 1] {
			mut client, mut sender := header_test_pair(is_server)!
			mut frames := []u8{}
			if invalid_sequence == 0 {
				// A final continuation without an open fragmented message.
				frames << header_test_frame([]u8{}, is_server, 0x80)
			} else {
				// A new data frame cannot interrupt an open fragmented message.
				frames << header_test_frame([u8(`a`)], is_server, 0x01)
				frames << header_test_frame([u8(`b`)], is_server, 0x81)
			}
			assert sender.write(frames)! == frames.len
			if _ := client.read_next_message() {
				assert false, 'invalid fragmented message sequence must fail'
			} else {
				assert err.msg().contains('continuation') || err.msg().contains('fragmented message')
			}
			header_test_assert_close_code(mut sender, is_server, 1002)!
			sender.close()!
		}
	}
}

fn test_reader_limits_fragmented_message_and_allows_interleaved_control() ! {
	mut client, mut sender := header_test_pair(true)!
	client.max_message_bytes = 5
	first := header_test_frame('abc'.bytes(), true, 0x01)
	assert sender.write(first)! == first.len
	oversized_continuation := header_test_declared_frame(3, true, 0x80)
	assert sender.write(oversized_continuation)! == oversized_continuation.len
	if _ := client.read_next_message() {
		assert false, 'aggregate fragment size must be rejected before payload read'
	} else {
		assert err.msg().contains('configured size limit')
	}
	header_test_assert_close_code(mut sender, true, 1009)!
	sender.close()!

	mut valid_client, mut valid_sender := header_test_pair(true)!
	valid_client.max_message_bytes = 5
	frames := [header_test_frame('ab'.bytes(), true, 0x01),
		header_test_frame('cd'.bytes(), true, 0x00),
		header_test_frame([]u8{len: 125, init: 7}, true, 0x89),
		header_test_frame('e'.bytes(), true, 0x80)]
	mut writer := spawn header_test_write(mut valid_sender, frames, false)
	ping := valid_client.read_next_message()!
	assert ping.opcode == .ping
	assert ping.payload == []u8{len: 125, init: 7}
	assert valid_client.fragments.len == 1
	assert valid_client.fragments[0].data.bytestr() == 'abcd'
	message := valid_client.read_next_message()!
	assert message.opcode == .text_frame
	assert message.payload.bytestr() == 'abcde'
	writer.wait()!
	valid_client.conn.close()!
}

fn test_reader_keeps_empty_fragment_metadata_bounded() ! {
	mut client, mut sender := header_test_pair(true)!
	client.max_message_bytes = 1
	mut frames := [header_test_frame([]u8{}, true, 0x01), header_test_frame([]u8{}, true, 0x89)]
	for _ in 0 .. 256 {
		frames << header_test_frame([]u8{}, true, 0x00)
		frames << header_test_frame([]u8{}, true, 0x89)
	}
	frames << header_test_frame([]u8{}, true, 0x80)
	mut writer := spawn header_test_write(mut sender, frames, false)
	for _ in 0 .. 257 {
		ping := client.read_next_message()!
		assert ping.opcode == .ping
		assert client.fragments.len == 1
		assert client.fragments[0].data.len == 0
	}
	message := client.read_next_message()!
	assert message.opcode == .text_frame
	assert message.payload.len == 0
	assert client.fragments.len == 0
	writer.wait()!
	client.conn.close()!
}

fn test_fragment_append_grows_and_clears_noslices_flag() {
	mut fragment := Fragment{
		data:   []u8{len: 4, init: 65}
		opcode: .binary_frame
	}
	fragment.append_payload([u8(66)])
	assert fragment.data.len == 5
	for i in 0 .. 4 {
		assert fragment.data[i] == 65
	}
	assert fragment.data[4] == 66
	assert fragment.data.cap > 4
	assert !fragment.data.flags.has(.noslices)
	unsafe { fragment.data.free() }
}

fn test_reader_preserves_many_grown_fragment_messages() ! {
	mut client, mut sender := header_test_pair(true) or { return error('pair failed: ${err}') }
	defer { client.conn.close() or {} }
	mut frames := [][]u8{cap: 256 * 3}
	for i in 0 .. 256 {
		first := []u8{len: 2, init: u8(i)}
		middle := [u8(i)]
		last := [u8(i)]
		frames << header_test_frame(first, true, 0x02)
		frames << header_test_frame(middle, true, 0x00)
		frames << header_test_frame(last, true, 0x80)
		unsafe { first.free() }
	}
	mut writer := spawn header_test_write(mut sender, frames, false)
	mut fragment_capacity := 0
	for i in 0 .. 256 {
		message := client.read_next_message()!
		assert message.opcode == .binary_frame
		assert message.payload.len == 4
		if i == 0 {
			fragment_capacity = client.fragments.cap
			assert fragment_capacity > 0
		} else {
			assert client.fragments.cap == fragment_capacity
		}
		for j in 0 .. 4 {
			assert message.payload[j] == u8(i)
		}
		assert client.fragments.len == 0
		unsafe { message.payload.free() }
	}
	writer.wait()!
	for frame in frames {
		unsafe { frame.free() }
	}
	unsafe { frames.free() }
}
