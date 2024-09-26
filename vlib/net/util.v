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
	port := addr.all_after_last(':').int()
	mut address := addr.all_before_last(':')

	// TODO(emily): Maybe do some more checking here
	// to validate ipv6 address sanity?

	// RFC4038 - allow [::1]:port
	if address.len > 0 && address[0] == `[` && address[address.len - 1] == `]` {
		address = address[1..address.len - 1]
	}

	p := validate_port(port)!
	return address, p
}
