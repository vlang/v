module quic

// test_multi_stream_interleaved_reassembly_and_flow_control is Phase 6's
// capstone integration test (explicitly named in the plan): simulates
// STREAM frames for THREE different streams -- a client-opened bidi
// stream, a server-opened uni stream (exercising "even client-first phase
// must correctly receive server-initiated unidirectional streams from day
// one"), and a second client-opened bidi stream -- arriving genuinely
// interleaved (not grouped by stream), routed through one QuicStreamSet,
// each independently reassembled, while a single connection-level
// ReceiveWindow tracks the running total across all three at once.
fn test_multi_stream_interleaved_reassembly_and_flow_control() {
	mut set := new_quic_stream_set(.client)
	mut conn_window := new_receive_window(1000)
	mut conn_total := u64(0)
	max_streams := u64(10)

	// stream_id 0: client-initiated bidi ("Hello" + " World")
	// stream_id 3: server-initiated uni ("Foo" + "Bar") -- recv-only for us
	// stream_id 4: client-initiated bidi ("X")
	//
	// Streams 0 and 4 are CLIENT-initiated: this client must have already
	// opened them itself (get_or_create deliberately refuses to fabricate
	// a locally-initiated stream just because a frame references it --
	// see its own doc comment and test_quic_stream_set_get_or_create_
	// rejects_locally_initiated_unknown_id). Stream 3 is server-initiated,
	// so get_or_create's peer-driven auto-creation is exactly the
	// intended path for it.
	mut s0 := set.open_local_stream(.bidirectional) // becomes id 0
	mut s4 := set.open_local_stream(.bidirectional) // becomes id 4

	// Delivered in a genuinely interleaved order -- not grouped by stream,
	// and stream 3's FIN-bearing second chunk is processed before stream
	// 0's own second chunk.

	// 1: stream 3, "Foo" (no fin)
	mut s3 := set.get_or_create(3, max_streams)!
	assert s3.has_recv()
	s3.recv.note_data(0, 'Foo'.bytes())!
	conn_total += 3
	conn_window.note_received(conn_total)!

	// 2: stream 0, "Hello" (no fin)
	s0.recv.note_data(0, 'Hello'.bytes())!
	conn_total += 5
	conn_window.note_received(conn_total)!

	// 3: stream 4, "X" (with fin -- single complete message)
	s4.recv.note_data(0, 'X'.bytes())!
	s4.recv.note_size_known(1)!
	conn_total += 1
	conn_window.note_received(conn_total)!

	// 4: stream 3, "Bar" (with fin)
	s3.recv.note_data(3, 'Bar'.bytes())!
	s3.recv.note_size_known(6)!
	conn_total += 3
	conn_window.note_received(conn_total)!

	// 5: stream 0, " World" (with fin)
	s0.recv.note_data(5, ' World'.bytes())!
	s0.recv.note_size_known(11)!
	conn_total += 6
	conn_window.note_received(conn_total)!

	// Each stream reassembled its own data correctly, independent of the
	// others and independent of arrival order.
	assert s0.recv.reassembler.data().bytestr() == 'Hello World'
	assert s0.recv.state == .data_recvd

	assert s3.recv.reassembler.data().bytestr() == 'FooBar'
	assert s3.recv.state == .data_recvd
	assert !s3.has_send() // confirmed recv-only, as a peer-initiated uni stream must be

	assert s4.recv.reassembler.data().bytestr() == 'X'
	assert s4.recv.state == .data_recvd

	// The connection-level window correctly tracked the SUM across all
	// three streams ('Hello'=5 + 'Foo'=3 + 'X'=1 + 'Bar'=3 + ' World'=6 =
	// 18 bytes), not just whichever stream was touched last.
	assert conn_total == 18
	assert conn_window.advertised_limit() == 1000
}
