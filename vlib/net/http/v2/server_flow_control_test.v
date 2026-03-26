module v2

// Tests for server outbound flow control (RFC 7540 §6.9).

fn test_outbound_flow_control_default_values() {
	fc := OutboundFlowControl{}
	assert fc.connection_window == 65535, 'default connection window should be 65535'
	assert fc.stream_windows.len == 0, 'stream windows should be empty initially'
}

fn test_outbound_flow_control_init_stream() {
	mut fc := OutboundFlowControl{}
	fc.init_stream(u32(1), u32(65535))
	assert fc.stream_windows[u32(1)] == i64(65535), 'stream 1 window should be 65535'
}

fn test_outbound_flow_control_update_connection_window() {
	mut fc := OutboundFlowControl{}
	fc.update_connection_window(u32(1000))!
	assert fc.connection_window == i64(65535 + 1000), 'connection window should increase by increment'
}

fn test_outbound_flow_control_update_stream_window() {
	mut fc := OutboundFlowControl{}
	fc.init_stream(u32(1), u32(65535))
	fc.update_stream_window(u32(1), u32(500))!
	assert fc.stream_windows[u32(1)] == i64(65535 + 500), 'stream window should increase by increment'
}

fn test_outbound_flow_control_zero_increment_connection_error() {
	mut fc := OutboundFlowControl{}
	fc.update_connection_window(u32(0)) or {
		assert err.msg().contains('PROTOCOL_ERROR'), 'zero increment should be PROTOCOL_ERROR'
		return
	}
	assert false, 'zero increment should return error'
}

fn test_outbound_flow_control_zero_increment_stream_error() {
	mut fc := OutboundFlowControl{}
	fc.init_stream(u32(3), u32(65535))
	fc.update_stream_window(u32(3), u32(0)) or {
		assert err.msg().contains('PROTOCOL_ERROR'), 'zero increment should be PROTOCOL_ERROR'
		return
	}
	assert false, 'zero increment should return error'
}

fn test_outbound_flow_control_connection_window_overflow() {
	mut fc := OutboundFlowControl{}
	fc.connection_window = 0x7fffffff - 10
	fc.update_connection_window(u32(20)) or {
		assert err.msg().contains('FLOW_CONTROL_ERROR'), 'overflow should be FLOW_CONTROL_ERROR'
		return
	}
	assert false, 'overflow should return error'
}

fn test_outbound_flow_control_stream_window_overflow() {
	mut fc := OutboundFlowControl{}
	fc.init_stream(u32(1), u32(65535))
	fc.stream_windows[u32(1)] = 0x7fffffff - 5
	fc.update_stream_window(u32(1), u32(10)) or {
		assert err.msg().contains('FLOW_CONTROL_ERROR'), 'overflow should be FLOW_CONTROL_ERROR'
		return
	}
	assert false, 'overflow should return error'
}

fn test_outbound_flow_control_available_window() {
	mut fc := OutboundFlowControl{}
	fc.connection_window = 1000
	fc.init_stream(u32(1), u32(65535))
	fc.stream_windows[u32(1)] = 500
	assert fc.available_window(u32(1)) == i64(500), 'available should be min of connection and stream'

	fc.stream_windows[u32(1)] = 2000
	assert fc.available_window(u32(1)) == i64(1000), 'available should be min of connection and stream'
}

fn test_outbound_flow_control_available_window_unknown_stream() {
	mut fc := OutboundFlowControl{}
	fc.connection_window = 1000
	assert fc.available_window(u32(99)) == i64(0), 'unknown stream should return 0'
}

fn test_outbound_flow_control_consume() {
	mut fc := OutboundFlowControl{}
	fc.connection_window = 1000
	fc.init_stream(u32(1), u32(65535))
	fc.stream_windows[u32(1)] = 800
	fc.consume(u32(1), i64(300))
	assert fc.connection_window == i64(700), 'connection window should decrease'
	assert fc.stream_windows[u32(1)] == i64(500), 'stream window should decrease'
}

fn test_outbound_flow_control_adjust_initial_window_size() {
	mut fc := OutboundFlowControl{}
	fc.init_stream(u32(1), u32(65535))
	fc.init_stream(u32(3), u32(65535))
	fc.adjust_initial_window_size(u32(65535), u32(131070))
	assert fc.stream_windows[u32(1)] == i64(131070), 'stream 1 should be adjusted by delta'
	assert fc.stream_windows[u32(3)] == i64(131070), 'stream 3 should be adjusted by delta'
}

fn test_outbound_flow_control_adjust_initial_window_size_decrease() {
	mut fc := OutboundFlowControl{}
	fc.init_stream(u32(1), u32(65535))
	fc.adjust_initial_window_size(u32(65535), u32(32767))
	assert fc.stream_windows[u32(1)] == i64(32767), 'stream 1 should decrease by delta'
}

fn test_outbound_flow_control_remove_stream() {
	mut fc := OutboundFlowControl{}
	fc.init_stream(u32(1), u32(65535))
	fc.init_stream(u32(3), u32(65535))
	fc.remove_stream(u32(1))
	assert u32(1) !in fc.stream_windows, 'stream 1 should be removed'
	assert u32(3) in fc.stream_windows, 'stream 3 should remain'
}
