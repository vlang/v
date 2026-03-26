module v2

// Per-connection context: outbound flow control and shared handler state.
import sync

// OutboundFlowControl tracks the peer's advertised flow control windows for
// server-side outbound DATA frames per RFC 7540 §6.9.
// All methods are mutex-protected because the flow control state is accessed
// from both the frame loop thread and spawned dispatch_stream goroutines.
struct OutboundFlowControl {
mut:
	mu                sync.Mutex
	connection_window i64 = 65535
	stream_windows    map[u32]i64
}

// init_stream registers a new stream with its initial window size.
fn (mut fc OutboundFlowControl) init_stream(stream_id u32, initial_size u32) {
	fc.mu.lock()
	fc.stream_windows[stream_id] = i64(initial_size)
	fc.mu.unlock()
}

// update_connection_window increases the connection-level window by increment.
fn (mut fc OutboundFlowControl) update_connection_window(increment u32) ! {
	if increment == 0 {
		return error('PROTOCOL_ERROR: WINDOW_UPDATE increment must not be 0 (RFC 7540 §6.9.1)')
	}
	fc.mu.lock()
	new_size := fc.connection_window + i64(increment)
	if new_size > 0x7fffffff {
		fc.mu.unlock()
		return error('FLOW_CONTROL_ERROR: connection window exceeds 2^31-1 (RFC 7540 §6.9.1)')
	}
	fc.connection_window = new_size
	fc.mu.unlock()
}

// update_stream_window increases a stream-level window by increment.
fn (mut fc OutboundFlowControl) update_stream_window(stream_id u32, increment u32) ! {
	if increment == 0 {
		return error('PROTOCOL_ERROR: WINDOW_UPDATE increment must not be 0 (RFC 7540 §6.9.1)')
	}
	fc.mu.lock()
	current := fc.stream_windows[stream_id] or {
		fc.mu.unlock()
		return
	}
	new_size := current + i64(increment)
	if new_size > 0x7fffffff {
		fc.mu.unlock()
		return error('FLOW_CONTROL_ERROR: stream window exceeds 2^31-1 (RFC 7540 §6.9.1)')
	}
	fc.stream_windows[stream_id] = new_size
	fc.mu.unlock()
}

// available_window returns the effective send window for a stream.
fn (mut fc OutboundFlowControl) available_window(stream_id u32) i64 {
	fc.mu.lock()
	stream_win := fc.stream_windows[stream_id] or {
		fc.mu.unlock()
		return 0
	}
	result := if fc.connection_window < stream_win {
		fc.connection_window
	} else {
		stream_win
	}
	fc.mu.unlock()
	return result
}

// consume decreases both connection and stream windows after sending data.
fn (mut fc OutboundFlowControl) consume(stream_id u32, amount i64) {
	fc.mu.lock()
	fc.connection_window -= amount
	if _ := fc.stream_windows[stream_id] {
		fc.stream_windows[stream_id] = fc.stream_windows[stream_id] - amount
	}
	fc.mu.unlock()
}

// adjust_initial_window_size adjusts all stream windows by the delta between
// old and new INITIAL_WINDOW_SIZE per RFC 7540 §6.9.2.
fn (mut fc OutboundFlowControl) adjust_initial_window_size(old_size u32, new_size u32) {
	delta := i64(new_size) - i64(old_size)
	fc.mu.lock()
	for stream_id, window in fc.stream_windows {
		fc.stream_windows[stream_id] = window + delta
	}
	fc.mu.unlock()
}

// check_initial_window_overflow validates that adjusting stream windows by
// the delta between old_size and new_size will not overflow 2^31-1 per
// RFC 7540 §6.9.2. Must be called before adjust_initial_window_size.
fn (mut fc OutboundFlowControl) check_initial_window_overflow(old_size u32, new_size u32) ! {
	delta := i64(new_size) - i64(old_size)
	fc.mu.lock()
	for _, window in fc.stream_windows {
		if window + delta > 0x7fffffff {
			fc.mu.unlock()
			return error('FLOW_CONTROL_ERROR: stream window exceeds 2^31-1 after INITIAL_WINDOW_SIZE adjustment (RFC 7540 §6.9.2)')
		}
	}
	fc.mu.unlock()
}

// remove_stream deletes the window entry for a closed stream.
fn (mut fc OutboundFlowControl) remove_stream(stream_id u32) {
	fc.mu.lock()
	fc.stream_windows.delete(stream_id)
	fc.mu.unlock()
}

struct ConnContext {
mut:
	encoder  Encoder
	write_mu sync.Mutex
	wg       sync.WaitGroup
	flow     OutboundFlowControl
}
