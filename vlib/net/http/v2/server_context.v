// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import sync

// OutboundFlowControl tracks the peer's advertised flow control windows for
// server-side outbound DATA frames per RFC 7540 §6.9.
// connection_window is the connection-level window; stream_windows holds
// per-stream windows keyed by stream ID.
struct OutboundFlowControl {
mut:
	connection_window i64 = 65535
	stream_windows    map[u32]i64
}

// init_stream registers a new stream's outbound window using the given initial size.
fn (mut fc OutboundFlowControl) init_stream(stream_id u32, initial_size u32) {
	fc.stream_windows[stream_id] = i64(initial_size)
}

// update_connection_window increases the connection-level window by increment.
// Returns PROTOCOL_ERROR if increment is 0, FLOW_CONTROL_ERROR if overflow.
fn (mut fc OutboundFlowControl) update_connection_window(increment u32) ! {
	if increment == 0 {
		return error('PROTOCOL_ERROR: WINDOW_UPDATE increment must not be 0 (RFC 7540 §6.9.1)')
	}
	new_size := fc.connection_window + i64(increment)
	if new_size > 0x7fffffff {
		return error('FLOW_CONTROL_ERROR: connection window exceeds 2^31-1 (RFC 7540 §6.9.1)')
	}
	fc.connection_window = new_size
}

// update_stream_window increases a stream's window by increment.
// Returns PROTOCOL_ERROR if increment is 0, FLOW_CONTROL_ERROR if overflow.
fn (mut fc OutboundFlowControl) update_stream_window(stream_id u32, increment u32) ! {
	if increment == 0 {
		return error('PROTOCOL_ERROR: WINDOW_UPDATE increment must not be 0 (RFC 7540 §6.9.1)')
	}
	current := fc.stream_windows[stream_id] or { return }
	new_size := current + i64(increment)
	if new_size > 0x7fffffff {
		return error('FLOW_CONTROL_ERROR: stream window exceeds 2^31-1 (RFC 7540 §6.9.1)')
	}
	fc.stream_windows[stream_id] = new_size
}

// available_window returns the effective window for a stream: min of connection and stream.
// Returns 0 for unknown streams.
fn (fc &OutboundFlowControl) available_window(stream_id u32) i64 {
	stream_win := fc.stream_windows[stream_id] or { return 0 }
	if fc.connection_window < stream_win {
		return fc.connection_window
	}
	return stream_win
}

// consume decrements both connection and stream windows after sending data.
fn (mut fc OutboundFlowControl) consume(stream_id u32, amount i64) {
	fc.connection_window -= amount
	if _ := fc.stream_windows[stream_id] {
		fc.stream_windows[stream_id] = fc.stream_windows[stream_id] - amount
	}
}

// adjust_initial_window_size adjusts all open stream windows by the delta
// when the peer changes SETTINGS_INITIAL_WINDOW_SIZE (RFC 7540 §6.9.2).
fn (mut fc OutboundFlowControl) adjust_initial_window_size(old_size u32, new_size u32) {
	delta := i64(new_size) - i64(old_size)
	for stream_id, window in fc.stream_windows {
		fc.stream_windows[stream_id] = window + delta
	}
}

// remove_stream removes a stream's window tracking entry.
fn (mut fc OutboundFlowControl) remove_stream(stream_id u32) {
	fc.stream_windows.delete(stream_id)
}

// ConnContext holds shared per-connection state needed by spawned handlers.
// Heap-allocated so spawned threads can safely reference it.
struct ConnContext {
mut:
	encoder  Encoder
	write_mu sync.Mutex
	wg       sync.WaitGroup
	flow     OutboundFlowControl
}
