module quic

// RFC 9000 §4 — Flow Control. Both connection- and stream-level limits
// apply SIMULTANEOUSLY and independently: a frame within its own stream's
// window can still be blocked by the connection-level aggregate window,
// and vice versa -- a caller must always check/update BOTH the relevant
// stream's window and the connection-level window for every STREAM frame,
// never just one.

// FlowControlWindow tracks one DIRECTION's flow-control accounting for
// what THIS endpoint may SEND, at one scope (a whole connection, or a
// single stream): how many bytes have been consumed against a limit, and
// what that limit currently is (raised over time by the PEER's
// MAX_DATA/MAX_STREAM_DATA frames).
pub struct FlowControlWindow {
mut:
	consumed u64
	limit    u64
}

pub fn new_flow_control_window(initial_limit u64) FlowControlWindow {
	return FlowControlWindow{
		limit: initial_limit
	}
}

// available returns how many more bytes this window currently permits.
pub fn (w &FlowControlWindow) available() u64 {
	if w.consumed >= w.limit {
		return 0
	}
	return w.limit - w.consumed
}

// consume records `n` more bytes as used against this window, failing if
// that would exceed the current limit -- callers must check available()
// (or catch this error) BEFORE actually sending data, never discover the
// violation only after the fact.
pub fn (mut w FlowControlWindow) consume(n u64) ! {
	if n > w.available() {
		return error('quic: flow control window exceeded: attempted to consume ${n} bytes, only ${w.available()} available (limit ${w.limit}, consumed ${w.consumed})')
	}
	w.consumed += n
}

// raise_limit updates the window's limit, e.g. on receiving the peer's
// MAX_DATA/MAX_STREAM_DATA frame. Per RFC 9000 §4.1, a limit update MUST
// NOT be applied if it is SMALLER than the current limit (limits are
// monotonically non-decreasing) -- silently ignored, not an error, since a
// reordered older MAX_DATA/MAX_STREAM_DATA frame arriving after a newer
// one is entirely normal, not a protocol violation.
pub fn (mut w FlowControlWindow) raise_limit(new_limit u64) {
	if new_limit > w.limit {
		w.limit = new_limit
	}
}

// ReceiveWindow tracks how much data THIS endpoint is willing to RECEIVE
// (its own advertised limit to the peer) and how much of it the
// application has actually consumed, deciding when to advertise a higher
// limit. RFC 9000 §4.1 recommends sending updates before the window is
// fully exhausted, not only once it hits zero, to avoid a throughput
// stall while the peer waits for permission to keep sending.
pub struct ReceiveWindow {
mut:
	received      u64 // bytes actually received so far (network progress)
	read          u64 // bytes the application has consumed (frees window)
	advertised    u64 // the limit we've told the peer via MAX_DATA/MAX_STREAM_DATA
	initial_limit u64
}

pub fn new_receive_window(initial_limit u64) ReceiveWindow {
	return ReceiveWindow{
		advertised:    initial_limit
		initial_limit: initial_limit
	}
}

pub fn (w &ReceiveWindow) advertised_limit() u64 {
	return w.advertised
}

// note_received records that the peer has sent data up to
// `new_total_received` (a cumulative offset, not a delta), checking
// against the CURRENTLY advertised limit -- a peer exceeding what we
// advertised is a FLOW_CONTROL_ERROR. Non-regressing: an out-of-order
// frame reporting a smaller cumulative total than already recorded is not
// an error, just a no-op (the larger total already reflects it).
pub fn (mut w ReceiveWindow) note_received(new_total_received u64) ! {
	if new_total_received > w.advertised {
		return error('quic: FLOW_CONTROL_ERROR: peer sent data up to offset ${new_total_received}, exceeding the advertised limit of ${w.advertised}')
	}
	if new_total_received > w.received {
		w.received = new_total_received
	}
}

// note_read records that the application has consumed up to
// `new_total_read` (a cumulative offset).
pub fn (mut w ReceiveWindow) note_read(new_total_read u64) {
	if new_total_read > w.read {
		w.read = new_total_read
	}
}

// should_advertise_more reports whether it's time to raise and send a new
// MAX_DATA/MAX_STREAM_DATA limit to the peer: once the application has
// consumed at least half of the currently-advertised window. A simple,
// standard auto-tuning heuristic that keeps the peer from ever actually
// hitting zero available window in ordinary steady-state use (avoiding a
// throughput stall) while still bounding how much unread data this
// endpoint commits to buffering at once.
pub fn (w &ReceiveWindow) should_advertise_more() bool {
	return w.read >= w.advertised / 2
}

// next_advertised_limit returns the new limit to advertise, once
// should_advertise_more() is true -- extends the window by another
// initial_limit's worth. The caller sends the corresponding MAX_DATA/
// MAX_STREAM_DATA frame and then calls mark_advertised to commit it.
pub fn (w &ReceiveWindow) next_advertised_limit() u64 {
	return w.advertised + w.initial_limit
}

pub fn (mut w ReceiveWindow) mark_advertised(new_limit u64) {
	if new_limit > w.advertised {
		w.advertised = new_limit
	}
}

// initial_send_limit_for_stream returns the initial flow-control limit
// THIS endpoint may send on `id`, given the PEER's own advertised
// transport parameters (RFC 9000 §4.1). The naming is peer-relative and
// easy to get backwards: `initial_max_stream_data_bidi_local` in the
// PEER's parameters describes streams THEY consider local (streams THEY
// initiate) -- which are REMOTE-initiated from where we're sitting.
// Conversely their `_bidi_remote` describes streams WE initiate. This
// function resolves that inversion once, in one place, rather than
// leaving every call site to get the direction right on its own.
//
// Concretely, for role=client: on a client-initiated bidi stream (ours to
// send on), the limit is the SERVER's initial_max_stream_data_bidi_remote
// (the server's own "how much may streams opened by my peer send me"
// value). On a server-initiated bidi stream, it's the server's
// initial_max_stream_data_bidi_local (the server's own "how much may my
// peer send me on streams I opened" value).
pub fn initial_send_limit_for_stream(id StreamId, role QuicRole, peer_params QuicTransportParameters) u64 {
	locally_initiated := id.is_locally_initiated(role)
	if id.direction() == .unidirectional {
		return peer_params.initial_max_stream_data_uni or { 0 }
	}
	return if locally_initiated {
		peer_params.initial_max_stream_data_bidi_remote or { 0 }
	} else {
		peer_params.initial_max_stream_data_bidi_local or { 0 }
	}
}

// initial_receive_limit_for_stream returns the initial flow-control limit
// THIS endpoint has advertised to the PEER for how much the PEER may send
// on `id`, given OUR OWN transport parameters (the ones we sent). Mirror
// image of initial_send_limit_for_stream, using our own parameters
// directly (no inversion needed here -- they're already from our own
// perspective).
pub fn initial_receive_limit_for_stream(id StreamId, role QuicRole, own_params QuicTransportParameters) u64 {
	locally_initiated := id.is_locally_initiated(role)
	if id.direction() == .unidirectional {
		return own_params.initial_max_stream_data_uni or { 0 }
	}
	return if locally_initiated {
		own_params.initial_max_stream_data_bidi_local or { 0 }
	} else {
		own_params.initial_max_stream_data_bidi_remote or { 0 }
	}
}
