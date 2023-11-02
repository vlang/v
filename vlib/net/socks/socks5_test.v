module socks

fn ipv4_socks() ! {
	mut v := socks5_dial('127.0.0.1:9150', '1.1.1.1:80', '', '')!
	assert v != unsafe { nil }
	v.close()!
}

fn domain_socks() ! {
	mut v := socks5_dial('127.0.0.1:9150', 'ifconfig.info:80', '', '')!
	assert v != unsafe { nil }
	v.close()!
}

fn test_parse_ipv4() {
	assert parse_ipv4('255.255.255.255')! == [u8(255), 255, 255, 255]
	assert parse_ipv4('127.0.0.1')! == [u8(127), 0, 0, 1]
	parse_ipv4('1.2..2') or { assert err.msg() == 'Ip address not valid' }
}
