module websocket

import net

struct InputBorrowState {
mut:
	retained [][]u8
}

fn input_borrow_message(mut _client ReactorClient, message &Message, ref voidptr) {
	mut state := unsafe { &InputBorrowState(ref) }
	state.retained << message.payload.clone()
}

fn input_borrow_wire(text string, opcode u8) []u8 {
	assert text.len <= 125
	mask := [u8(3), 7, 11, 13]!
	mut wire := [opcode, u8(0x80 | text.len), 3, 7, 11, 13]
	for i, byte in text.bytes() { wire << byte ^ mask[i % 4] }
	return wire
}

fn test_reactor_parse_reuses_input_after_delivery_and_partial_tail() ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	mut tcp := net.dial_tcp(listener.addr()!.str())!
	mut accepted := listener.accept()!
	listener.close()!
	state := &InputBorrowState{}
	mut reactor := new_reactor(on_message: input_borrow_message, user: state)!
	client := reactor.attach(mut accepted, '')!
	reactor.drain_inbox()
	mut socket := reactor.sockets[client.key] or { panic('missing attachment') }
	socket.input = []u8{cap: 256}
	buffer := socket.input.data
	assert reactor.parse(mut socket, 16) == 0
	mut expected := [][]u8{}
	for turn in 0 .. 30 {
		text := 'message ${turn}'
		wire := input_borrow_wire(text, 0x81)
		socket.input << wire
		// A trailing incomplete frame must survive deletion of the first frame.
		socket.input << wire[..3]
		assert reactor.parse(mut socket, 16) == 1
		assert socket.input.len == 3
		assert socket.input.data == buffer && socket.input.cap == 256
		socket.input << wire[3..]
		assert reactor.parse(mut socket, 16) == 1
		assert socket.input.len == 0
		assert socket.input.data == buffer && socket.input.cap == 256
		expected << text.bytes()
		expected << text.bytes()
		assert state.retained == expected
	}
	// Decoder-owned fragment storage and frame-budget rescheduling still work.
	socket.input << input_borrow_wire('hel', 0x01)
	socket.input << input_borrow_wire('lo', 0x80)
	assert reactor.parse(mut socket, 1) == 1
	assert state.retained == expected
	assert socket.input.data == buffer
	assert reactor.parse(mut socket, 1) == 1
	expected << 'hello'.bytes()
	assert state.retained == expected
	assert socket.input.data == buffer && socket.input.len == 0
	tcp.close()!
	reactor.stop()
	reactor.run()!
}
