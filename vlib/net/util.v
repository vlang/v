module net

const (
	socket_max_port = u16(0xFFFF)
)

// validate_port checks whether a port is valid
// and returns the port or an error
pub fn validate_port(port int) ?u16 {
	if port <= socket_max_port {
		return u16(port)
	} else {
		return err_port_out_of_range
	}
}

// split address splits an address into its host name and its port
pub fn split_address(addr string) ?(string, u16) {
	port := addr.all_after_last(':').int()
	address := addr.all_before_last(':')

	p := validate_port(port)?
	return address, p
}
