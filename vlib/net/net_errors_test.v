import net

fn wrap_error_is_ok(c int) bool {
	net.wrap_error(c) or { return false }
	return true
}

fn test_wrap_error_accepts_zero() {
	assert wrap_error_is_ok(0)
}

fn test_wrap_error_reports_the_socket_error() {
	// 1 is not a WSA code, and on the POSIX side the code is rendered directly,
	// so both platforms produce the same message.
	net.wrap_error(1) or {
		assert err.msg() == 'net: socket error: 1'
		assert err.code() == 1
		return
	}
	assert false, 'wrap_error(1) should have failed'
}

fn test_wrap_error_renders_a_known_windows_socket_code() {
	$if windows {
		net.wrap_error(10057) or {
			assert err.msg() == 'net: socket error: wsaenotconn'
			assert err.code() == 10057
			return
		}
		assert false, 'wrap_error(10057) should have failed'
	} $else {
		net.wrap_error(10057) or {
			assert err.msg() == 'net: socket error: 10057'
			assert err.code() == 10057
			return
		}
		assert false, 'wrap_error(10057) should have failed'
	}
}

fn test_socket_error_message_passes_non_negative_codes_through() {
	assert net.socket_error_message(0, 'ignored')! == 0
	assert net.socket_error_message(7, 'ignored')! == 7
}

fn test_socket_error_message_appends_the_caller_context() {
	// The code is negative, so the underlying socket error is fetched and the
	// caller's context is appended to whatever message it produced.
	net.socket_error_message(-1, 'peer_addr_from_socket_handle failed') or {
		assert err.msg().starts_with('net: socket error:')
		assert err.msg().ends_with('; peer_addr_from_socket_handle failed')
		return
	}
	assert false, 'socket_error_message(-1) should have failed'
}

$if windows {
	fn test_wsa_error_casts_known_windows_socket_codes() {
		assert net.wsa_error(10057) == .wsaenotconn
		assert net.wsa_error(10035) == .wsaewouldblock
		assert int(net.wsa_error(10057)) == 10057
		// An unknown code round-trips as the number it was given.
		assert net.wsa_error(1).str() == '1'
	}
}
