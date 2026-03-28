module http

fn test_set_server_only_header_overwrites_client_supplied_value_case_insensitively() {
	mut header := new_custom_header_from_map({
		'remote-addr': 'spoofed'
		'Remote-Addr': 'also-spoofed'
	}) or {
		assert false, err.msg()
		return
	}
	set_server_only_header(mut header, 'Remote-Addr', '127.0.0.1')
	assert header.custom_values('remote-addr').len == 1
	assert header.get_custom('remote-addr') or { '' } == '127.0.0.1'
}
