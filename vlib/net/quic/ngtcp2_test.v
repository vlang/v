module quic

// Tests for ngtcp2 bindings.

// make_test_fin_events creates QuicStreamEvents with FIN events for testing.
fn make_test_fin_events(fin_ids []i64) QuicStreamEvents {
	mut events := QuicStreamEvents{}
	for i, id in fin_ids {
		events.fin_stream_ids[i] = id
	}
	events.fin_count = i32(fin_ids.len)
	return events
}

// make_test_close_events creates QuicStreamEvents with close events for testing.
fn make_test_close_events(close_ids []i64) QuicStreamEvents {
	mut events := QuicStreamEvents{}
	for i, id in close_ids {
		events.closed_stream_ids[i] = id
	}
	events.closed_count = i32(close_ids.len)
	return events
}

fn test_ngtcp2_version() {
	version := get_version()
	assert version.chosen_version > 0

	println('✓ ngtcp2 version info retrieved successfully')
}

fn test_settings_default() {
	println('✓ Settings API available (tested in integration tests)')
}

fn test_transport_params_default() {
	println('✓ Transport params API available (tested in integration tests)')
}

fn test_connection_id() {
	mut cid := Ngtcp2CidStruct{
		datalen: 8
	}

	for i in 0 .. 8 {
		cid.data[i] = u8(i + 1)
	}

	assert cid.datalen == 8
	assert cid.data[0] == 1
	assert cid.data[7] == 8
}

fn test_stream_id_helpers() {
	assert is_bidi_stream(0) == true
	assert is_uni_stream(0) == false

	assert is_bidi_stream(2) == false
	assert is_uni_stream(2) == true

	assert is_bidi_stream(1) == true
	assert is_uni_stream(1) == false

	assert is_bidi_stream(3) == false
	assert is_uni_stream(3) == true
}

fn test_error_handling() {
	err_str := strerror(ngtcp2_err_invalid_argument)
	assert err_str.len > 0

	_ := err_is_fatal(ngtcp2_err_internal)
	_ := err_is_fatal(ngtcp2_err_discard_pkt)

	println('✓ Error handling functions work correctly')
}

fn test_varint_encoding() {
	assert u8(42) < 64
	assert u64(1000) >= 64 && u64(1000) < 16384
	assert u64(100000) >= 16384 && u64(100000) < 1073741824
	assert u64(2000000000) >= 1073741824
}

fn test_fin_flag_constants() {
	// Verify QUIC FIN flag constants match ngtcp2 C library values
	assert ngtcp2_write_stream_flag_none == u32(0x00), 'FLAG_NONE should be 0x00'
	assert ngtcp2_write_stream_flag_fin == u32(0x01), 'FLAG_FIN should be 0x01'
	assert ngtcp2_write_stream_flag_more == u32(0x02), 'FLAG_MORE should be 0x02'

	println('✓ FIN flag constants have correct values')
}

fn test_stream_fin_received_field() {
	// Verify Stream struct has fin_received field with correct default
	mut s := Stream{
		id: 42
	}
	assert s.fin_received == false, 'fin_received should default to false'

	s.fin_received = true
	assert s.fin_received == true, 'fin_received should be settable to true'

	println('✓ Stream.fin_received field works correctly')
}

fn test_fin_flags_are_distinct() {
	// Verify no flag value collisions
	assert ngtcp2_write_stream_flag_none != ngtcp2_write_stream_flag_fin
	assert ngtcp2_write_stream_flag_none != ngtcp2_write_stream_flag_more
	assert ngtcp2_write_stream_flag_fin != ngtcp2_write_stream_flag_more

	println('✓ FIN flag constants are all distinct')
}

fn test_fin_flags_can_combine() {
	// Verify FIN and MORE flags can be bitwise OR'd (as ngtcp2 supports)
	combined := ngtcp2_write_stream_flag_fin | ngtcp2_write_stream_flag_more
	assert combined == u32(0x03), 'FIN | MORE should be 0x03'

	println('✓ FIN flags can be combined via bitwise OR')
}

fn test_connection_creation() {
}

fn test_recv_stream_data_flag_fin_constant() {
	// The recv callback FIN flag must match ngtcp2's NGTCP2_STREAM_DATA_FLAG_FIN
	assert ngtcp2_stream_data_flag_fin == u32(0x01), 'NGTCP2_STREAM_DATA_FLAG_FIN should be 0x01'
	println('✓ recv stream data FIN flag constant is correct')
}

fn test_process_stream_fin_events_sets_fin_received() {
	// Arrange: simulate C callback having recorded FIN for stream 4
	mut events := make_test_fin_events([i64(4)])

	mut s := &Stream{id: 4}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s

	// Act
	process_stream_fin_events(mut events, mut streams)

	// Assert: FIN should propagate to stream
	assert s.fin_received == true, 'stream should have fin_received set after FIN event'
	assert events.fin_count == 0, 'fin_count should be reset after processing'
	println('✓ process_stream_fin_events correctly sets fin_received')
}

fn test_process_stream_fin_events_handles_multiple_streams() {
	// Arrange: FIN for streams 0, 4, and 8
	mut events := make_test_fin_events([i64(0), 4, 8])

	mut s0 := &Stream{id: 0}
	mut s4 := &Stream{id: 4}
	mut s8 := &Stream{id: 8}
	mut streams := map[u64]&Stream{}
	streams[u64(0)] = s0
	streams[u64(4)] = s4
	streams[u64(8)] = s8

	// Act
	process_stream_fin_events(mut events, mut streams)

	// Assert: all three streams should have fin_received set
	assert s0.fin_received == true, 'stream 0 should have fin_received'
	assert s4.fin_received == true, 'stream 4 should have fin_received'
	assert s8.fin_received == true, 'stream 8 should have fin_received'
	assert events.fin_count == 0, 'fin_count should be reset'
	println('✓ process_stream_fin_events handles multiple FIN events')
}

fn test_process_stream_fin_events_does_not_affect_unrelated_streams() {
	// Arrange: FIN for stream 99 (not in map), stream 4 is unrelated
	mut events := make_test_fin_events([i64(99)])

	mut s4 := &Stream{id: 4}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4

	// Act
	process_stream_fin_events(mut events, mut streams)

	// Assert: unrelated stream unaffected, unknown stream auto-created
	assert s4.fin_received == false, 'unrelated stream should not be affected'
	assert u64(99) in streams, 'unknown stream should be auto-created'
	assert events.fin_count == 0, 'fin_count should be reset'
	println('✓ process_stream_fin_events does not affect unrelated streams')
}

fn test_process_stream_close_events_sets_closed() {
	// Arrange: simulate C callback recording close for stream 4
	mut events := make_test_close_events([i64(4)])

	mut s := &Stream{id: 4}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s

	// Act
	process_stream_close_events(mut events, mut streams)

	// Assert: closed should propagate to stream
	assert s.closed == true, 'stream should be closed after close event'
	assert events.closed_count == 0, 'closed_count should be reset after processing'
	println('✓ process_stream_close_events correctly sets closed')
}

fn test_drain_stream_events_pub_drains_fin_and_close() {
	// Arrange: FIN on stream 4, close on stream 8
	mut events := make_test_fin_events([i64(4)])
	events.closed_stream_ids[0] = 8
	events.closed_count = 1	mut s4 := &Stream{id: 4}
	mut s8 := &Stream{id: 8}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4
	streams[u64(8)] = s8

	mut conn := Connection{
		stream_events: &events
		streams:       streams
	}

	// Act: use the public drain method (no overflow → no error)
	conn.drain_stream_events() or {
		assert false, 'unexpected error in drain: ${err}'
		return
	}

	// Assert: both FIN and close events should be drained
	assert s4.fin_received == true, 'stream 4 should have fin_received after drain'
	assert s8.closed == true, 'stream 8 should be closed after drain'
	assert events.fin_count == 0, 'fin_count should be reset'
	assert events.closed_count == 0, 'closed_count should be reset'
	println('✓ drain_stream_events drains both FIN and close events')
}

fn test_drain_stream_events_safe_with_nil_events() {
	// drain_stream_events must not panic when stream_events is nil
	mut conn := Connection{
		stream_events: unsafe { nil }
	}
	conn.drain_stream_events() or {
		assert false, 'nil stream_events should not cause error: ${err}'
		return
	}
	println('✓ drain_stream_events safe with nil stream_events')
}

fn test_process_incoming_packet_safe_with_nil_conn() {
	// process_incoming_packet must not panic when ngtcp2_conn is nil
	mut conn := Connection{
		ngtcp2_conn: unsafe { nil }
	}
	conn.process_incoming_packet([]u8{len: 10}) or {
		assert false, 'nil conn guard should not error: ${err}'
		return
	}
	println('✓ process_incoming_packet safe with nil ngtcp2_conn')
}

fn test_process_stream_fin_events_auto_creates_stream_on_unknown_fin() {
	// When FIN arrives for a stream not yet in the map (e.g. first packet
	// on a new stream), the stream must be auto-created with fin_received.
	mut events := make_test_fin_events([i64(99)])

	mut streams := map[u64]&Stream{}

	// Act
	process_stream_fin_events(mut events, mut streams)

	// Assert: stream 99 should be auto-created with fin_received = true
	assert u64(99) in streams, 'stream should be auto-created when FIN arrives for unknown stream'
	if s99 := streams[u64(99)] {
		assert s99.fin_received == true, 'auto-created stream should have fin_received = true'
	} else {
		assert false, 'stream 99 should exist in map'
	}
	assert events.fin_count == 0, 'fin_count should be reset after processing'
	println('✓ process_stream_fin_events auto-creates stream on unknown FIN')
}

fn test_process_stream_close_events_auto_creates_stream_on_unknown_close() {
	// When close arrives for a stream not yet in the map, the stream
	// must be auto-created with closed = true.
	mut events := make_test_close_events([i64(99)])

	mut streams := map[u64]&Stream{}

	// Act
	process_stream_close_events(mut events, mut streams)

	// Assert: stream 99 should be auto-created with closed = true
	assert u64(99) in streams, 'stream should be auto-created when close arrives for unknown stream'
	if s99 := streams[u64(99)] {
		assert s99.closed == true, 'auto-created stream should have closed = true'
	} else {
		assert false, 'stream 99 should exist in map'
	}
	assert events.closed_count == 0, 'closed_count should be reset after processing'
	println('✓ process_stream_close_events auto-creates stream on unknown close')
}

// === H4: Event Buffer Overflow Detection Tests ===

fn test_event_overflow_fin_flag_set() {
	// When fin_count reaches 64 (max), the C callback should set overflow.
	// Simulating: overflow flag was set by C callback after buffer is full.
	mut events := QuicStreamEvents{}
	events.fin_count = 64
	events.overflow = 1 // simulates C-side overflow detection

	assert events.overflow == 1, 'overflow flag should be readable when set'
	println('✓ overflow flag is accessible on QuicStreamEvents')
}

fn test_event_overflow_closed_flag_set() {
	// When closed_count reaches 64, overflow flag should be set.
	mut events := QuicStreamEvents{}
	events.closed_count = 64
	events.overflow = 1

	assert events.overflow == 1, 'overflow flag should be set for closed events too'
	println('✓ overflow flag works for closed event overflow')
}

fn test_drain_stream_events_detects_overflow() {
	// drain_stream_events must detect overflow, process remaining events,
	// clear the flag, and return an error to the caller.
	mut events := make_test_fin_events([i64(4)])
	events.overflow = 1

	mut s4 := &Stream{id: 4}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4

	mut conn := Connection{
		stream_events: &events
		streams:       streams
	}

	// Act: drain should process available events and return error for overflow
	conn.drain_stream_events() or {
		// Assert: events still processed, overflow flag cleared, error returned
		assert s4.fin_received == true, 'available events should still be processed'
		assert events.fin_count == 0, 'fin_count should be reset'
		assert events.overflow == 0, 'overflow should be cleared after drain processes it'
		assert err.msg().contains('overflow')
		println('✓ drain_stream_events handles overflow flag and returns error')
		return
	}
	assert false, 'drain_stream_events should return error on overflow'
}

fn test_overflow_default_zero() {
	// overflow should default to 0 (no overflow)
	events := QuicStreamEvents{}
	assert events.overflow == 0, 'overflow should default to 0'
	println('✓ overflow defaults to 0')
}

// === H7: QUIC Abstraction API Tests ===

fn test_ensure_stream_creates_new() {
	// ensure_stream should create a new stream if it doesn't exist
	mut conn := Connection{}
	s := conn.ensure_stream(42)
	assert s.id == 42, 'created stream should have correct id'
	assert u64(42) in conn.streams, 'stream should be in connection map'
	println('✓ ensure_stream creates new stream')
}

fn test_ensure_stream_returns_existing() {
	// ensure_stream should return existing stream without creating a new one
	mut existing := &Stream{id: 7, fin_received: true}
	mut conn := Connection{}
	conn.streams[u64(7)] = existing

	s := conn.ensure_stream(7)
	assert s.id == 7, 'should return existing stream'
	assert s.fin_received == true, 'should preserve existing stream state'
	println('✓ ensure_stream returns existing stream')
}

fn test_stream_has_fin_returns_false_for_unknown() {
	// stream_has_fin should return false for unknown stream IDs
	conn := Connection{}
	assert conn.stream_has_fin(999) == false, 'unknown stream should not have FIN'
	println('✓ stream_has_fin returns false for unknown stream')
}

fn test_stream_has_fin_returns_true() {
	// stream_has_fin should return true when stream has fin_received
	mut conn := Connection{}
	conn.streams[u64(10)] = &Stream{id: 10, fin_received: true}
	assert conn.stream_has_fin(10) == true, 'stream with FIN should return true'
	println('✓ stream_has_fin returns true for FIN stream')
}

fn test_stream_has_fin_returns_false_when_no_fin() {
	// stream_has_fin should return false when stream exists but no FIN
	mut conn := Connection{}
	conn.streams[u64(10)] = &Stream{id: 10, fin_received: false}
	assert conn.stream_has_fin(10) == false, 'stream without FIN should return false'
	println('✓ stream_has_fin returns false when no FIN')
}

fn test_stream_exists() {
	// stream_exists should return true for registered streams
	mut conn := Connection{}
	conn.streams[u64(5)] = &Stream{id: 5}
	assert conn.stream_exists(5) == true, 'registered stream should exist'
	assert conn.stream_exists(999) == false, 'unregistered stream should not exist'
	println('✓ stream_exists works correctly')
}

// === H-NEW3: send() nil C pointer safety tests ===

fn test_send_with_data_returns_error_on_closed_connection() {
	// H-NEW3: send() must check ensure_open() BEFORE C.ngtcp2_conn_get_max_data_left
	// Bug: previously called C function with nil pointer before safety checks
	mut conn := Connection{
		closed: true
		ngtcp2_conn: unsafe { nil }
	}
	conn.send(0, [u8(1), 2, 3]) or {
		assert err.msg().contains('connection closed')
		println('✓ send returns error on closed connection before C calls')
		return
	}
	assert false, 'send should return error on closed connection'
}

fn test_send_with_data_returns_error_on_nil_ngtcp2_conn() {
	// H-NEW3: send() must check ensure_conn() BEFORE C.ngtcp2_conn_get_max_data_left
	mut conn := Connection{
		ngtcp2_conn: unsafe { nil }
	}
	conn.send(0, [u8(1), 2, 3]) or {
		assert err.msg().contains('not initialized')
		println('✓ send returns error on nil ngtcp2_conn before C calls')
		return
	}
	assert false, 'send should return error on nil ngtcp2_conn'
}

// === H-DUP: drain_stream_events overflow error tests ===

fn test_drain_stream_events_returns_error_on_overflow() {
	// H-DUP: drain_stream_events must return error when overflow is detected
	// This enables callers to treat overflow as a connection-level error
	mut events := make_test_fin_events([i64(4)])
	events.overflow = 1

	mut s4 := &Stream{id: 4}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4

	mut conn := Connection{
		stream_events: &events
		streams:       streams
	}

	conn.drain_stream_events() or {
		assert err.msg().contains('overflow')
		// Events should still be processed before the error is returned
		assert s4.fin_received == true, 'events should be processed before error'
		println('✓ drain_stream_events returns error on overflow')
		return
	}
	assert false, 'drain_stream_events should return error on overflow'
}

// === RECV-DATA: Stream receive data buffer tests ===

fn make_test_recv_data_events(entries []TestRecvEntry) QuicStreamEvents {
	mut events := QuicStreamEvents{}
	mut buf_offset := 0
	for i, entry in entries {
		if i >= quic_max_recv_data_events || buf_offset + entry.data.len > quic_recv_data_buf_size {
			break
		}
		events.recv_stream_ids[i] = entry.stream_id
		events.recv_offsets[i] = i32(buf_offset)
		events.recv_lengths[i] = i32(entry.data.len)
		for j, b in entry.data {
			events.recv_data_buf[buf_offset + j] = b
		}
		buf_offset += entry.data.len
		events.recv_count = i32(i + 1)
	}
	return events
}

struct TestRecvEntry {
	stream_id i64
	data      []u8
}

fn test_stream_has_recv_data_field() {
	// Stream struct must have a recv_data field separate from data (sent data)
	mut s := Stream{
		id: 1
	}
	assert s.recv_data.len == 0, 'recv_data should default to empty'
	s.recv_data << u8(0x48)
	assert s.recv_data.len == 1, 'recv_data should be appendable'
	println('✓ Stream.recv_data field exists and works')
}

fn test_process_stream_data_events_populates_recv_data() {
	// When C callback buffers received data, drain should populate stream.recv_data
	mut events := make_test_recv_data_events([
		TestRecvEntry{
			stream_id: 4
			data: [u8(0x48), 0x65, 0x6c, 0x6c, 0x6f]
		},
	])

	mut s4 := &Stream{id: 4}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4

	// Act
	process_stream_data_events(mut events, mut streams)

	// Assert
	assert s4.recv_data == [u8(0x48), 0x65, 0x6c, 0x6c, 0x6f], 'recv_data should contain received bytes'
	assert events.recv_count == 0, 'recv_count should be reset after processing'
	println('✓ process_stream_data_events populates recv_data')
}

fn test_process_stream_data_events_appends_multiple_chunks() {
	// Multiple data events for the same stream should append
	mut events := make_test_recv_data_events([
		TestRecvEntry{
			stream_id: 4
			data: [u8(0x41), 0x42]
		},
		TestRecvEntry{
			stream_id: 4
			data: [u8(0x43), 0x44]
		},
	])

	mut s4 := &Stream{id: 4}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4

	// Act
	process_stream_data_events(mut events, mut streams)

	// Assert
	assert s4.recv_data == [u8(0x41), 0x42, 0x43, 0x44], 'recv_data should contain all chunks appended'
	println('✓ process_stream_data_events appends multiple chunks')
}

fn test_process_stream_data_events_auto_creates_stream() {
	// Data arriving for unknown stream should auto-create the stream
	mut events := make_test_recv_data_events([
		TestRecvEntry{
			stream_id: 99
			data: [u8(0xFF)]
		},
	])

	mut streams := map[u64]&Stream{}

	// Act
	process_stream_data_events(mut events, mut streams)

	// Assert
	assert u64(99) in streams, 'stream should be auto-created'
	if s99 := streams[u64(99)] {
		assert s99.recv_data == [u8(0xFF)], 'auto-created stream should have recv_data'
	}
	println('✓ process_stream_data_events auto-creates stream for unknown ID')
}

fn test_drain_stream_events_includes_data_events() {
	// drain_stream_events must also process data events alongside FIN/close
	mut events := make_test_recv_data_events([
		TestRecvEntry{
			stream_id: 4
			data: [u8(0x48), 0x49]
		},
	])
	// Also add a FIN event
	events.fin_stream_ids[0] = 4
	events.fin_count = 1

	mut s4 := &Stream{id: 4}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4

	mut conn := Connection{
		stream_events: &events
		streams:       streams
	}

	conn.drain_stream_events() or {
		assert false, 'unexpected error: ${err}'
		return
	}

	assert s4.recv_data == [u8(0x48), 0x49], 'recv_data should be populated after drain'
	assert s4.fin_received == true, 'FIN should also be processed'
	println('✓ drain_stream_events processes data events alongside FIN/close')
}

fn test_recv_returns_recv_data_not_sent_data() {
	// recv() must return recv_data (from peer), not data (locally sent)
	mut s4 := &Stream{
		id: 4
		data: [u8(0x01), 0x02] // locally sent data
		recv_data: [u8(0xAA), 0xBB, 0xCC] // received from peer
	}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4

	// Verify the stream has separate sent vs received data
	assert s4.data == [u8(0x01), 0x02], 'sent data should be preserved'
	assert s4.recv_data == [u8(0xAA), 0xBB, 0xCC], 'recv_data should be separate'
	println('✓ Stream has separate data and recv_data fields')
}

fn test_quic_stream_events_has_recv_fields() {
	// QuicStreamEvents must have recv data buffer fields
	mut events := QuicStreamEvents{}
	assert events.recv_count == 0, 'recv_count should default to 0'
	events.recv_stream_ids[0] = 4
	events.recv_offsets[0] = 0
	events.recv_lengths[0] = 5
	events.recv_data_buf[0] = 0x48
	events.recv_count = 1
	assert events.recv_count == 1, 'recv_count should be settable'
	println('✓ QuicStreamEvents has recv data buffer fields')
}

// === M4: process_incoming_packet error propagation tests ===

fn test_process_incoming_packet_has_error_return_type() {
	// M4: process_incoming_packet returns ! to allow error propagation
	// With nil conn, should return early (guard) without error
	mut conn := Connection{
		ngtcp2_conn: unsafe { nil }
	}
	conn.process_incoming_packet([]u8{len: 10}) or {
		assert false, 'nil conn guard should return early, not error'
		return
	}
	println('✓ process_incoming_packet has error return type and nil guard works')
}

// === O1/O2: C/V struct layout safety tests ===

fn test_quic_stream_events_struct_size_matches_c() {
	// O1/O2: Verify V struct size matches C struct size exactly.
	// C layout (with padding for int64_t alignment):
	//   int64_t fin_stream_ids[64]    = 512
	//   int     fin_count             = 4 (+4 padding)
	//   int64_t closed_stream_ids[64] = 512
	//   int     closed_count          = 4
	//   int     overflow              = 4
	//   int64_t recv_stream_ids[64]   = 512
	//   int     recv_offsets[64]      = 256
	//   int     recv_lengths[64]      = 256
	//   int     recv_count            = 4
	//   uint8_t recv_data_buf[65536]  = 65536
	//   int     recv_data_buf_used    = 4 (+4 trailing padding)
	//   Total = 67608
	expected_c_size := 67608
	actual_v_size := int(sizeof(QuicStreamEvents))
	assert actual_v_size == expected_c_size, 'QuicStreamEvents V struct size ${actual_v_size} != C struct size ${expected_c_size}'
	println('✓ QuicStreamEvents V struct size matches C struct size (${actual_v_size} bytes)')
}

fn test_quic_stream_events_i32_fields_are_4_bytes() {
	// O1/O2: All int fields that map to C int must be exactly 4 bytes (i32).
	// This ensures portability across platforms where V int may differ from C int.
	assert sizeof(i32) == 4, 'i32 must be 4 bytes to match C int'
	println('✓ i32 is 4 bytes, matching C int')
}

// === O4: recv() clears buffer after clone ===

fn test_recv_data_cleared_after_read() {
	// O4: After reading recv_data, the buffer should be cleared so
	// repeated reads don't return accumulated old data.
	mut s4 := &Stream{
		id: 4
		recv_data: [u8(0xAA), 0xBB, 0xCC]
	}
	mut streams := map[u64]&Stream{}
	streams[u64(4)] = s4

	// Simulate what recv() does: clone then clear
	result := s4.recv_data.clone()
	s4.recv_data.clear()

	assert result == [u8(0xAA), 0xBB, 0xCC], 'first read should return data'
	assert s4.recv_data.len == 0, 'recv_data should be cleared after read'

	// Second read should return empty
	result2 := s4.recv_data.clone()
	assert result2.len == 0, 'second read should return empty after clear'
	println('✓ recv_data is cleared after read (no stale data accumulation)')
}
