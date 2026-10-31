module websocket

import net
import os
import sync.stdatomic
import time

struct ReactorTestState {
mut:
	opened   u64
	closed   u64
	accepted u64
	codes    chan int = chan int{cap: 4096}
}

fn reactor_test_open(mut _c ReactorClient, ref voidptr) {
	mut state := unsafe { &ReactorTestState(ref) }
	stdatomic.add_u64(&state.opened, 1)
}

fn reactor_test_closed(mut _c ReactorClient, code int, _reason string, ref voidptr) {
	mut state := unsafe { &ReactorTestState(ref) }
	stdatomic.add_u64(&state.closed, 1)
	state.codes <- code
}

fn reactor_test_echo(mut client ReactorClient, message &Message, _ref voidptr) {
	client.write(message.payload, message.opcode) or { panic(err) }
}

fn reactor_test_wire(payload []u8, opcode u8) []u8 {
	assert payload.len <= 125
	mut wire := [opcode, u8(payload.len) | 0x80, u8(3), 7, 11, 13]
	mask := [u8(3), 7, 11, 13]
	for i, byte in payload { wire << byte ^ mask[i % 4] }
	return wire
}

fn reactor_test_pair() !(&net.TcpConn, &net.TcpConn) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut client := net.dial_tcp(listener.addr()!.str())!
	mut accepted := listener.accept()!
	client.set_read_timeout(3 * time.second)
	return client, accepted
}

fn reactor_test_eof(mut conn net.TcpConn) {
	started := time.sys_mono_now()
	mut byte := u8(0)
	for time.sys_mono_now() - started < 3 * time.second {
		n := C.recv(conn.sock.handle, &byte, 1, C.MSG_DONTWAIT)
		if n == 0 { return }
		assert n < 0 && C.errno in [C.EAGAIN, C.EWOULDBLOCK, C.EINTR]
		time.sleep(time.millisecond)
	}
	assert false, 'expected TCP EOF'
}

fn reactor_test_code(state &ReactorTestState) int {
	select {
		code := <-state.codes {
			return code
		}
		3 * time.second {
			assert false, 'missing terminal callback'
		}
	}
	return 0
}

fn test_reactor_reserves_pending_capacity_and_preserves_rejected_ownership() ! {
	mut a, mut sa := reactor_test_pair()!
	mut b, mut sb := reactor_test_pair()!
	defer {
		a.close() or {}
		b.close() or {}
		sb.close() or {}
	}
	state := &ReactorTestState{}
	mut reactor := new_reactor(
		max_connections: 1
		on_open:         reactor_test_open
		on_close:        reactor_test_closed
		user:            state
	)!
	reactor.attach(mut sa, '')!
	mut rejected := false
	reactor.attach(mut sb, '') or { rejected = err.msg().contains('connection limit') }
	assert rejected
	// The rejected socket is still ours and remains usable.
	sb.write('owned'.bytes())!
	mut bytes := []u8{len: 5}
	assert b.read(mut bytes)! == 5 && bytes == 'owned'.bytes()
	reactor.stop()
	reactor.run()!
	assert stdatomic.load_u64(&state.opened) == 0
	assert stdatomic.load_u64(&state.closed) == 1
	assert reactor_test_code(state) == 1001
	reactor_test_eof(mut a)
	mut twice := false
	reactor.run() or { twice = true }
	assert twice
}

fn test_reactor_async_attachment_failure_has_one_terminal_callback() ! {
	state := &ReactorTestState{}
	mut reactor := new_reactor(
		max_connections: 1
		on_open:         reactor_test_open
		on_close:        reactor_test_closed
		user:            state
	)!
	mut bad := &net.TcpConn{ sock: net.TcpSocket{ Socket: net.Socket{ handle: -1 } }, handle: -1 }
	reactor.attach(mut bad, '')!
	mut worker := spawn reactor.run()
	assert reactor_test_code(state) == 1006
	assert stdatomic.load_u64(&state.opened) == 0
	mut client, mut accepted := reactor_test_pair()!
	defer { client.close() or {} }
	reactor.attach(mut accepted, '')!
	client.close()!
	assert reactor_test_code(state) == 1006
	reactor.stop()
	worker.wait()!
	assert stdatomic.load_u64(&state.closed) == 2
}

fn test_reactor_waits_for_normal_close_reply_and_rejects_new_sends() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	defer { tcp.close() or {} }
	state := &ReactorTestState{}
	mut reactor := new_reactor(on_close: reactor_test_closed, user: state)!
	mut worker := spawn reactor.run()
	mut client := reactor.attach(mut accepted, '')!
	client.write_string('before close')!
	client.close(1000, 'done')!
	mut rejected := false
	client.write_string('after close') or { rejected = true }
	assert rejected
	mut receiver := &Client{ conn: tcp, client_state: ClientState{ state: .open } }
	assert receiver.read_next_message()!.payload.bytestr() == 'before close'
	close := receiver.read_next_message()!
	assert close.opcode == .close && close.payload == [u8(3), 232, `d`, `o`, `n`, `e`]
	mut byte := u8(0)
	assert C.recv(tcp.sock.handle, &byte, 1, C.MSG_DONTWAIT) < 0
	assert C.errno in [C.EAGAIN, C.EWOULDBLOCK]
	assert stdatomic.load_u64(&state.closed) == 0
	tcp.write(reactor_test_wire(close.payload, 0x88))!
	reactor_test_eof(mut tcp)
	assert reactor_test_code(state) == 1000
	reactor.stop()
	worker.wait()!
	assert stdatomic.load_u64(&state.closed) == 1
}

fn test_reactor_close_timeout_and_shutdown_are_bounded() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	defer { tcp.close() or {} }
	state := &ReactorTestState{}
	mut reactor := new_reactor(
		close_timeout: 100 * time.millisecond
		on_close:      reactor_test_closed
		user:          state
	)!
	mut worker := spawn reactor.run()
	mut client := reactor.attach(mut accepted, '')!
	client.write_string('ready')!
	mut receiver := &Client{ conn: tcp, client_state: ClientState{ state: .open } }
	assert receiver.read_next_message()!.payload.bytestr() == 'ready'
	started := time.sys_mono_now()
	reactor.stop()
	close := receiver.read_next_message()!
	assert close.opcode == .close && close.payload[..2] == [u8(3), 233]
	reactor_test_eof(mut tcp)
	worker.wait()!
	elapsed := time.sys_mono_now() - started
	assert elapsed >= 100 * time.millisecond && elapsed < time.second
	assert reactor_test_code(state) == 1006
	mut rejected := false
	client.write_string('after stop') or { rejected = true }
	assert rejected
	reactor.stop()
}

fn test_reactor_partial_writes_preserve_binary_and_text_frames() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	defer { tcp.close() or {} }
	accepted.sock.set_option_int(.send_buf_size, 4096)!
	mut reactor := new_reactor(max_pending_bytes: 4 * 1024 * 1024)!
	mut worker := spawn reactor.run()
	mut client := reactor.attach(mut accepted, '')!
	mut receiver := &Client{ conn: tcp, client_state: ClientState{ state: .open } }
	for i in 0 .. 3 { client.write([]u8{len: 500000 + i, init: u8(i)}, .binary_frame)! }
	time.sleep(50 * time.millisecond)
	for i in 0 .. 3 {
		message := receiver.read_next_message()!
		assert message.opcode == .binary_frame
		assert message.payload == []u8{len: 500000 + i, init: u8(i)}
	}
	client.write_string('final')!
	assert receiver.read_next_message()!.payload.bytestr() == 'final'
	tcp.close()!
	reactor.stop()
	worker.wait()!
}

fn test_reactor_frame_budget_reschedules_buffered_input() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	defer { tcp.close() or {} }
	mut reactor := new_reactor(frames_per_turn: 2, on_message: reactor_test_echo)!
	mut worker := spawn reactor.run()
	reactor.attach(mut accepted, '')!
	mut wire := []u8{}
	for i in 0 .. 40 { wire << reactor_test_wire('${i}'.bytes(), 0x81) }
	tcp.write(wire)!
	mut receiver := &Client{ conn: tcp, client_state: ClientState{ state: .open } }
	for i in 0 .. 40 {
		assert receiver.read_next_message()!.payload.bytestr() == '${i}'
	}
	tcp.close()!
	reactor.stop()
	worker.wait()!
}

fn test_reactor_empty_fragmented_messages_and_empty_binary_output() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	defer { tcp.close() or {} }
	mut reactor := new_reactor(on_message: reactor_test_echo)!
	mut worker := spawn reactor.run()
	mut client := reactor.attach(mut accepted, '')!
	mut receiver := &Client{ conn: tcp, client_state: ClientState{ state: .open } }
	for opcode in [u8(1), 2] {
		tcp.write(reactor_test_wire([]u8{}, opcode))!
		tcp.write(reactor_test_wire([]u8{}, 0x80))!
		message := receiver.read_next_message()!
		assert int(message.opcode) == int(opcode) && message.payload.len == 0
	}
	client.write([]u8{}, .binary_frame)!
	assert receiver.read_next_message()!.payload.len == 0
	tcp.close()!
	reactor.stop()
	worker.wait()!
}

fn reactor_test_producer(mut client ReactorClient, producer int) {
	for i in 0 .. 100 { client.write_string('${producer}:${i}') or { panic(err) } }
}

fn test_reactor_concurrent_producers_keep_per_producer_order() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	defer { tcp.close() or {} }
	state := &ReactorTestState{}
	mut reactor := new_reactor(
		max_pending_messages: 1024
		on_close:             reactor_test_closed
		user:                 state
	)!
	mut worker := spawn reactor.run()
	mut client := reactor.attach(mut accepted, '')!
	mut producers := []thread{}
	for producer in 0 .. 4 { producers << spawn reactor_test_producer(mut client, producer) }
	mut receiver := &Client{ conn: tcp, client_state: ClientState{ state: .open } }
	mut counts := [4]int{}
	for _ in 0 .. 400 {
		message := receiver.read_next_message()!.payload.bytestr().split(':')
		producer, sequence := message[0].int(), message[1].int()
		assert producer >= 0 && producer < 4
		assert sequence == counts[producer]
		counts[producer]++
	}
	producers.wait()
	assert counts == [100, 100, 100, 100]!
	tcp.close()!
	reactor.stop()
	worker.wait()!
	assert stdatomic.load_u64(&state.closed) == 1
}

fn test_reactor_mailbox_is_bounded_before_worker_starts() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	mut second, mut second_accepted := reactor_test_pair()!
	defer {
		tcp.close() or {}
		second.close() or {}
		second_accepted.close() or {}
	}
	state := &ReactorTestState{}
	mut reactor := new_reactor(max_commands: 1, on_close: reactor_test_closed, user: state)!
	mut client := reactor.attach(mut accepted, '')!
	mut rejected := false
	reactor.attach(mut second_accepted, '') or { rejected = true }
	assert rejected
	mut send_rejected := false
	client.write_string('full') or { send_rejected = true }
	assert send_rejected
	mut worker := spawn reactor.run()
	assert reactor_test_code(state) == 1013
	reactor_test_eof(mut tcp)
	reactor.stop()
	worker.wait()!
}

fn test_reactor_repeated_start_stop_releases_descriptors() ! {
	before := os.ls('/proc/self/fd')!.len
	for _ in 0 .. 30 {
		mut reactor := new_reactor()!
		reactor.stop()
		reactor.run()!
	}
	assert os.ls('/proc/self/fd')!.len == before
}

fn reactor_test_attach_racer(mut reactor Reactor, state &ReactorTestState) {
	for _ in 0 .. 80 {
		mut tcp, mut accepted := reactor_test_pair() or { panic(err) }
		mut client := reactor.attach(mut accepted, '') or {
			accepted.close() or {}
			tcp.close() or {}
			continue
		}
		stdatomic.add_u64(&state.accepted, 1)
		client.write_string('race') or {}
		client.close(1000, '') or {}
		tcp.close() or {}
	}
}

fn test_reactor_stop_racing_attach_send_close_accounts_for_every_socket() ! {
	before := os.ls('/proc/self/fd')!.len
	for _ in 0 .. 5 {
		state := &ReactorTestState{}
		mut reactor := new_reactor(on_close: reactor_test_closed, user: state)!
		mut worker := spawn reactor.run()
		mut producers := []thread{}
		for _ in 0 .. 4 { producers << spawn reactor_test_attach_racer(mut reactor, state) }
		time.sleep(10 * time.millisecond)
		reactor.stop()
		producers.wait()
		worker.wait()!
		assert stdatomic.load_u64(&state.closed) == stdatomic.load_u64(&state.accepted)
	}
	assert os.ls('/proc/self/fd')!.len == before
}

struct ReactorReentryState {
mut:
	inside bool
	closed bool
}

fn reactor_reentry_open(mut client ReactorClient, ref voidptr) {
	mut state := unsafe { &ReactorReentryState(ref) }
	state.inside = true
	// Overflow the byte budget from inside a callback. on_close must be deferred.
	for _ in 0 .. 3 { client.write_string('x'.repeat(110)) or {} }
	assert !state.closed
	state.inside = false
}

fn reactor_reentry_closed(mut client ReactorClient, code int, _reason string, ref voidptr) {
	mut state := unsafe { &ReactorReentryState(ref) }
	assert !state.inside && !state.closed && code == 1013
	state.closed = true
	client.close(1000, '') or {}
	client.owner.stop()
}

fn test_reactor_overload_callback_can_stop_without_recursive_close() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	defer { tcp.close() or {} }
	state := &ReactorReentryState{}
	mut reactor := new_reactor(
		max_pending_bytes: 128
		on_open:           reactor_reentry_open
		on_close:          reactor_reentry_closed
		user:              state
	)!
	reactor.attach(mut accepted, '')!
	reactor.run()!
	assert state.closed
}

fn test_reactor_write_timeout_tracks_progress_not_total_transfer_time() ! {
	mut tcp, mut accepted := reactor_test_pair()!
	defer { tcp.close() or {} }
	accepted.sock.set_option_int(.send_buf_size, 4096)!
	state := &ReactorTestState{}
	mut reactor := new_reactor(
		max_pending_bytes: 4 * 1024 * 1024
		write_timeout:     200 * time.millisecond
		on_close:          reactor_test_closed
		user:              state
	)!
	mut worker := spawn reactor.run()
	mut client := reactor.attach(mut accepted, '')!
	payload := []u8{len: 2 * 1024 * 1024, init: u8(index % 251)}
	client.write(payload, .binary_frame)!
	mut received := []u8{}
	mut buffer := []u8{len: 32768}
	started := time.sys_mono_now()
	for received.len < payload.len + 10 {
		n := tcp.read(mut buffer)!
		assert n > 0
		received << buffer[..n]
		time.sleep(10 * time.millisecond)
	}
	assert time.sys_mono_now() - started > 200 * time.millisecond
	assert received[..2] == [u8(0x82), 127]
	assert received[10..] == payload
	assert stdatomic.load_u64(&state.closed) == 0
	tcp.close()!
	reactor.stop()
	worker.wait()!
}
