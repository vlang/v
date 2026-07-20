module quic

fn test_flow_control_window_available_and_consume() {
	mut w := new_flow_control_window(100)
	assert w.available() == 100
	w.consume(60)!
	assert w.available() == 40
	w.consume(40)!
	assert w.available() == 0
}

fn test_flow_control_window_rejects_exceeding_limit() {
	mut w := new_flow_control_window(100)
	w.consume(90)!
	w.consume(20) or {
		assert err.msg().contains('flow control window exceeded')
		return
	}
	assert false, 'expected exceeding the window limit to be rejected'
}

fn test_flow_control_window_raise_limit_never_regresses() {
	mut w := new_flow_control_window(100)
	w.raise_limit(200)
	assert w.available() == 200
	w.raise_limit(150) // smaller/reordered update -- must be ignored, not an error
	assert w.available() == 200
}

fn test_receive_window_note_received_rejects_exceeding_advertised() {
	mut w := new_receive_window(100)
	w.note_received(100)!
	w.note_received(150) or {
		assert err.msg().contains('FLOW_CONTROL_ERROR')
		return
	}
	assert false, 'expected exceeding the advertised limit to be rejected'
}

fn test_receive_window_note_received_is_non_regressing() {
	mut w := new_receive_window(100)
	w.note_received(80)!
	w.note_received(50)! // reordered/duplicate frame reporting a smaller cumulative total
	assert w.advertised_limit() == 100
	// A subsequent note_received with the higher value again must still work.
	w.note_received(90)!
}

fn test_receive_window_auto_growth_heuristic() {
	mut w := new_receive_window(100)
	assert w.should_advertise_more() == false
	w.note_read(40)
	assert w.should_advertise_more() == false // less than half
	w.note_read(50)
	assert w.should_advertise_more() == true // >= half (50/100)

	next := w.next_advertised_limit()
	assert next == 200
	w.mark_advertised(next)
	assert w.advertised_limit() == 200
}

fn test_receive_window_mark_advertised_never_regresses() {
	mut w := new_receive_window(100)
	w.mark_advertised(200)
	assert w.advertised_limit() == 200
	w.mark_advertised(150)
	assert w.advertised_limit() == 200
}

// test_initial_stream_limits_hand_derived_client_perspective is a
// deliberately hand-derived check (not just structural) of
// initial_send_limit_for_stream/initial_receive_limit_for_stream's
// peer-relative naming inversion, from the CLIENT's own point of view --
// the shape of bug this needs to catch is the send/receive limits or the
// local/remote fields getting swapped, which a purely structural test
// (e.g. "returns SOME value") would never catch.
fn test_initial_stream_limits_hand_derived_client_perspective() {
	peer_params := QuicTransportParameters{
		initial_max_stream_data_bidi_local:  1000 // server's own: how much CLIENT may send on SERVER-opened bidi streams
		initial_max_stream_data_bidi_remote: 2000 // server's own: how much CLIENT may send on CLIENT-opened bidi streams
		initial_max_stream_data_uni:         3000 // server's own: how much CLIENT may send on CLIENT-opened uni streams
	}
	own_params := QuicTransportParameters{
		initial_max_stream_data_bidi_local:  4000 // client's own: how much SERVER may send on CLIENT-opened bidi streams
		initial_max_stream_data_bidi_remote: 5000 // client's own: how much SERVER may send on SERVER-opened bidi streams
		initial_max_stream_data_uni:         6000 // client's own: how much SERVER may send on SERVER-opened uni streams
	}

	client_bidi := StreamId{
		value: 0
	} // client-initiated bidi
	server_bidi := StreamId{
		value: 1
	} // server-initiated bidi
	client_uni := StreamId{
		value: 2
	} // client-initiated uni (send-only for the client)
	server_uni := StreamId{
		value: 3
	} // server-initiated uni (recv-only for the client)

	// SEND limits (how much the CLIENT may send), governed by the SERVER's
	// (peer's) parameters:
	assert initial_send_limit_for_stream(client_bidi, .client, peer_params) == 2000 // peer's _bidi_remote
	assert initial_send_limit_for_stream(server_bidi, .client, peer_params) == 1000 // peer's _bidi_local
	assert initial_send_limit_for_stream(client_uni, .client, peer_params) == 3000 // peer's _uni

	// RECEIVE limits (how much the CLIENT has told the server it may
	// send), governed by the CLIENT's OWN parameters:
	assert initial_receive_limit_for_stream(client_bidi, .client, own_params) == 4000 // own _bidi_local
	assert initial_receive_limit_for_stream(server_bidi, .client, own_params) == 5000 // own _bidi_remote
	assert initial_receive_limit_for_stream(server_uni, .client, own_params) == 6000 // own _uni
}

fn test_initial_stream_limits_default_to_zero_when_absent() {
	empty := QuicTransportParameters{}
	client_bidi := StreamId{
		value: 0
	}
	assert initial_send_limit_for_stream(client_bidi, .client, empty) == 0
	assert initial_receive_limit_for_stream(client_bidi, .client, empty) == 0
}

// test_connection_vs_stream_window_interplay is the plan's own explicitly
// named test: a frame within its own stream's window can still be
// blocked by the connection-level aggregate window, and vice versa --
// both must be checked independently, and satisfying one is never enough
// on its own.
fn test_connection_vs_stream_window_interplay() {
	mut stream_window := new_flow_control_window(1000) // plenty of room at the stream level
	mut conn_window := new_flow_control_window(100) // tight at the connection level

	// A send of 100 bytes fits comfortably within the stream's own window,
	// but must still be checked against (and correctly exhausts) the
	// tighter connection-level window.
	assert stream_window.available() >= 100
	assert conn_window.available() >= 100
	stream_window.consume(100)!
	conn_window.consume(100)!
	assert conn_window.available() == 0
	assert stream_window.available() == 900 // stream itself still has room

	// A further send that the STREAM window alone would happily allow
	// must still be rejected because the CONNECTION window is exhausted --
	// checking only the stream window would incorrectly permit it.
	conn_window.consume(1) or {
		assert err.msg().contains('flow control window exceeded')
		return
	}
	assert false, 'expected the connection-level window to block a send the stream window alone would allow'
}

fn test_connection_vs_stream_window_interplay_reverse() {
	// Mirror scenario: connection-level window has plenty of room, but
	// THIS stream's own window is tight -- the stream-level check must
	// independently block it even though the connection level wouldn't.
	mut stream_window := new_flow_control_window(50)
	mut conn_window := new_flow_control_window(10000)

	stream_window.consume(50)!
	conn_window.consume(50)!
	assert stream_window.available() == 0
	assert conn_window.available() == 9950

	stream_window.consume(1) or {
		assert err.msg().contains('flow control window exceeded')
		return
	}
	assert false, 'expected the stream-level window to block a send the connection window alone would allow'
}
