module net

// Well defined errors that are returned from socket functions
pub const (
	errors_base             = 0
	err_new_socket_failed   = error_with_code('net: new_socket failed to create socket',
		errors_base + 1)
	err_option_not_settable = error_with_code('net: set_option_xxx option not settable',
		errors_base + 2)
	err_option_wrong_type   = error_with_code('net: set_option_xxx option wrong type',
		errors_base + 3)
	err_port_out_of_range   = error_with_code('net: port out of range', errors_base + 5)
	err_no_udp_remote       = error_with_code('net: no udp remote', errors_base + 6)
	err_connect_failed      = error_with_code('net: connect failed', errors_base + 7)
	err_connect_timed_out   = error_with_code('net: connect timed out', errors_base + 8)
	err_timed_out           = error_with_code('net: op timed out', errors_base + 9)
	err_timed_out_code      = errors_base + 9
	err_connection_refused  = error_with_code('net: connection refused', errors_base + 10)
)

pub fn socket_error_message(potential_code int, s string) !int {
	return socket_error(potential_code) or { return error('${err.msg()}; ${s}') }
}

pub fn socket_error(potential_code int) !int {
	$if windows {
		if potential_code < 0 {
			last_error_int := C.WSAGetLastError()
			last_error := wsa_error(last_error_int)
			return error_with_code('net: socket error: (${last_error_int}) ${last_error}',
				int(last_error))
		}
	} $else {
		if potential_code < 0 {
			last_error := error_code()
			return error_with_code('net: socket error: ${last_error}', last_error)
		}
	}

	return potential_code
}

pub fn wrap_error(error_code int) ! {
	if error_code == 0 {
		return
	}
	$if windows {
		enum_error := wsa_error(error_code)
		return error_with_code('net: socket error: ${enum_error}', error_code)
	} $else {
		return error_with_code('net: socket error: ${error_code}', error_code)
	}
}

// wrap_read_result takes a read result and sees if it is 0 for graceful
// connection termination and returns none
// e.g. res := wrap_read_result(C.recv(c.sock.handle, voidptr(buf_ptr), len, 0))?
[inline]
fn wrap_read_result(result int) !int {
	if result == 0 {
		return error('none')
	}
	return result
}

[inline]
fn wrap_write_result(result int) !int {
	if result == 0 {
		return error('none')
	}
	return result
}
