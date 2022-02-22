module net

pub struct Socket {
	handle int
}

// address gets the address of a socket
pub fn (s &Socket) address() ?Addr {
	return addr_from_socket_handle(s.handle)
}
