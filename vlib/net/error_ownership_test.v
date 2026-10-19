// vtest vflags: -autofree
module net

fn test_invalid_ports_return_independent_errors() {
	for port in [-1, 65536, -2, 70000] {
		validate_port(port) or {
			assert err.code() == err_port_out_of_range.code()
			assert err.msg() == err_port_out_of_range.msg()
			continue
		}
		assert false, 'invalid port must return an error'
	}
	assert err_port_out_of_range.msg() == 'net: port out of range'
	assert err_port_out_of_range.code() == errors_base + 5
	assert validate_port(0)! == 0
	assert validate_port(65535)! == 65535
}

fn test_udp_without_remote_returns_independent_errors() {
	mut connection := listen_udp('127.0.0.1:0')!
	defer { connection.close() or {} }
	for _ in 0 .. 3 {
		connection.write_string('unused') or {
			assert err.code() == err_no_udp_remote.code()
			assert err.msg() == err_no_udp_remote.msg()
			continue
		}
		assert false, 'a listener has no remote address'
	}
	assert err_no_udp_remote.msg() == 'net: no udp remote'
}
