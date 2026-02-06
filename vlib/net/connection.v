module net

// Connection provides a generic SOCK_STREAM style interface that protocols can use
// as a base connection object to support TCP, UNIX Domain Sockets and various
// proxying solutions.
pub interface Connection {
	addr() !Addr
	peer_addr() !Addr
mut:
	read(mut []u8) !int
	write([]u8) !int
	close() !
}
