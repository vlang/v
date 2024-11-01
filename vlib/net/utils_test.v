import net

fn test_validate() {
	assert net.validate_port(0)! == 0
	assert net.validate_port(1)! == 1
	assert net.validate_port(0xFFFF)! == 0xFFFF
	if _ := net.validate_port(0xFFFF + 1) {
		assert false
	} else {
		assert true
	}
	if x := net.validate_port(-2) {
		dump(x)
		assert false
	} else {
		assert true
	}
}

fn test_resolve() {
	x := net.resolve_addrs_fuzzy('[::1]:10093', .udp)!
	assert x.len > 0
	assert x[0].str() == '[::1]:10093'
	assert x[0].port()! == 10093
}

fn test_resolve_port_without_brackets() {
	x := net.resolve_addrs_fuzzy('::1:48872', .udp)!
	assert x.len > 0
	assert x[0].port()! == 48872
}
