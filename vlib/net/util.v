module net

// validate_port checks whether a port is valid and returns the port or an error.
// The valid ports numbers are between 0 and 0xFFFF.
// For TCP, port number 0 is reserved and cannot be used, while for UDP, the source port
// is optional and a value of zero means no port.
// See also https://en.wikipedia.org/wiki/Port_%28computer_networking%29 .
pub fn validate_port(port int) !u16 {
	if port >= 0 && port <= 0xFFFF {
		return u16(port)
	} else {
		return err_port_out_of_range
	}
}

// split_address splits an address into its host name and its port
pub fn split_address(addr string) !(string, u16) {
	if _ := addr.index(']') {
		// ipv6 brackets
		address := addr.all_after('[').all_before_last(']')
		port := addr.all_after_last(']:').int()
		p := validate_port(port)!
		return address, p
	} else if _ := addr.index('::') {
		if addr.count(':') == 2 && addr.all_before_last('::') == '' {
			// ipv6 host only `::1`
			return addr, 0
		} else {
			// addr:port, or `::addr:port`
			address := addr.all_before_last(':')
			port := addr.all_after_last(':').int()
			p := validate_port(port)!
			return address, p
		}
	} else if _ := addr.index(':') {
		// addr:port
		address := addr.all_before_last(':')
		p := validate_port(addr.all_after_last(':').int())!
		return address, p
	} else {
		// addr only
		return addr, 0
	}
}
