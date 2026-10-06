module websocket

import net

fn test_reactor_payload_storage_copies_producer_bytes_and_reuses_two_buffers() ! {
	mut reactor := new_reactor(max_commands: 3, max_command_bytes: 128)!
	defer {
		reactor.stop()
		reactor.run() or { panic(err) }
	}
	mut client := &ReactorClient{ owner: reactor, key: 1 }
	mut buffers := []voidptr{}
	for turn in 0 .. 10 {
		mut bytes := []u8{len: 128, init: u8(index + turn)}
		retained := bytes.clone()
		client.write(bytes, .binary_frame)!
		for i in 0 .. bytes.len { bytes[i] = 0 }
		// Empty payloads consume a command slot but no byte budget.
		client.write([]u8{}, .binary_frame)!
		mut full := false
		client.write([u8(1)], .binary_frame) or { full = true }
		assert full
		lock reactor.inbox {
			assert reactor.inbox.payloads == retained
			assert reactor.inbox.bytes == 128
			assert reactor.inbox.commands.len == 2
			assert reactor.inbox.commands[0].text == ''
			assert reactor.inbox.commands[0].text_start == 0
			assert reactor.inbox.commands[0].text_len == 128
			assert reactor.inbox.commands[1].text_start == 128
			assert reactor.inbox.commands[1].text_len == 0
			if turn < 2 {
				buffers << reactor.inbox.payloads.data
			} else {
				assert reactor.inbox.payloads.data == buffers[turn % 2]
			}
		}
		reactor.drain_inbox()
		assert reactor.payload_spare.len == 0 && reactor.payload_spare.cap >= 128
		spare := reactor.payload_spare.data
		reactor.drain_inbox()
		assert reactor.payload_spare.data == spare
	}
	assert buffers[0] != buffers[1]
}

fn payload_storage_open(mut client ReactorClient, _ref voidptr) {
	// Force producer storage growth while the detached batch is being applied.
	client.write([u8(0), 255, 0], .binary_frame) or { panic(err) }
	client.write_string('b'.repeat(256)) or { panic(err) }
}

fn test_reactor_payload_storage_keeps_posted_batches_and_close_reason_owned() ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	mut tcp := net.dial_tcp(listener.addr()!.str())!
	mut accepted := listener.accept()!
	listener.close()!
	mut reactor := new_reactor(on_open: payload_storage_open)!
	mut client := reactor.attach(mut accepted, 'upgrade')!
	reactor.drain_inbox()
	mut socket := reactor.sockets[client.key] or { panic('missing attachment') }
	assert socket.output.bytestr() == 'upgrade'
	lock reactor.inbox {
		assert reactor.inbox.commands.len == 2 && reactor.inbox.bytes == 259
	}
	reactor.drain_inbox()
	assert socket.output[..12] == [u8(`u`), `p`, `g`, `r`, `a`, `d`, `e`, 0x82, 3, 0, 255, 0]
	assert socket.output[12..16] == [u8(0x81), 126, 1, 0]
	assert socket.output[16..].bytestr() == 'b'.repeat(256)
	client.close(1000, 'retained close reason')!
	reactor.drain_inbox()
	assert socket.close_reason == 'retained close reason'
	// Cycle both buffers through a missing connection to overwrite old payload bytes.
	mut missing := &ReactorClient{ owner: reactor, key: 999 }
	for _ in 0 .. 4 {
		missing.write_string('z'.repeat(512))!
		reactor.drain_inbox()
		assert socket.close_reason == 'retained close reason'
	}
	tcp.close()!
	reactor.stop()
	reactor.run()!
}
