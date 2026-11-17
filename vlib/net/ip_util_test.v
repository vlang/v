module net

fn test_is_ip() {
	assert is_ipv4('127.0.0.1')
	assert !is_ipv4('::1')
	assert is_ipv6('::1')
	assert is_ip('127.0.0.1')
	assert is_ip('::1')
	assert !is_ip('not an ip')
}
